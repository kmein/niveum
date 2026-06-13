#!/usr/bin/env python3
"""
Radcap.ru Stream Scraper - Simple Version
Uses only standard library modules
"""

import urllib.request
import urllib.error
import json
import re
from urllib.parse import urljoin
import time
from html.parser import HTMLParser

BASE_URL = "http://radcap.ru"
ICECAST_BASE = "http://79.111.119.111:8000"

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
}

def get_page(url):
    """Fetch a webpage with retry logic and timeout"""
    try:
        req = urllib.request.Request(url, headers=headers)
        with urllib.request.urlopen(req, timeout=15) as response:
            return response.read().decode('utf-8', errors='ignore')
    except (urllib.error.URLError, urllib.error.HTTPError, ConnectionError, TimeoutError) as e:
        print(f"Error fetching {url}: {e}")
        return None

def extract_links(html, pattern):
    """Extract links matching a pattern"""
    links = []
    if not html:
        return links

    # Simple regex to find links
    link_pattern = re.compile(r'href="([^"]+)"', re.IGNORECASE)
    matches = link_pattern.findall(html)

    for match in matches:
        if pattern in match:
            links.append(match)

    return links

def extract_genre_links():
    """Extract all genre links from the main index page"""
    print("Fetching main index page...")
    html = get_page(f"{BASE_URL}/index-d.html")
    if not html:
        return []

    # Look for genre links - they contain "genres-index-d" in class
    genre_links = []
    # More flexible pattern to catch different class formats
    pattern = re.compile(r'<a[^>]*class="[^"]*genres-index-d[^"]*"[^>]*href="([^"]+)"', re.IGNORECASE)
    matches = pattern.findall(html)

    # Also try to find links that look like genre pages
    genre_pattern = re.compile(r'<a[^>]*href="([^"]+\.html)"[^>]*class="[^"]*(genres-big|genres-index-d)[^"]*"', re.IGNORECASE)
    genre_matches = genre_pattern.findall(html)

    all_matches = matches + [match[0] for match in genre_matches]

    for match in all_matches:
        if match and not match.startswith('http') and match.endswith('.html'):
            full_url = urljoin(BASE_URL, match)
            # Exclude the "Все стили" link for now
            if 'index-db.html' not in full_url:
                genre_links.append(full_url)

    print(f"Found {len(genre_links)} genre links")
    return genre_links

def extract_station_links(genre_url):
    """Extract all station links from a genre page"""
    print(f"Fetching genre page: {genre_url}")
    html = get_page(genre_url)
    if not html:
        print("  Error: Could not fetch HTML")
        return []

    print(f"  HTML length: {len(html)} characters")

    station_links = []

    # Try different patterns for station links
    patterns = [
        r'<a[^>]*href="([^"]+\.html)"[^>]*class="[^"]*genres220[^"]*"',  # rock-d.html style
        r'<a[^>]*href="([^"]+\.html)"[^>]*class="[^"]*genres[^"]*"',     # ethnic-d.html style
    ]

    for pattern in patterns:
        regex = re.compile(pattern, re.IGNORECASE)
        matches = regex.findall(html)
        print(f"  Pattern found {len(matches)} matches")

        for match in matches:
            if match and not match.startswith('http') and not match.startswith('//'):
                full_url = urljoin(BASE_URL, match)
                if full_url not in station_links:
                    station_links.append(full_url)

    print(f"Found {len(station_links)} station links in {genre_url}")
    return station_links

def extract_stream_info(station_url):
    """Extract stream information from a station page"""
    print(f"Processing station: {station_url}")
    html = get_page(station_url)
    if not html:
        return None

    # Extract station title
    title_pattern = re.compile(r'<h2[^>]*class="title-stream1"[^>]*>(.*?)</h2>', re.IGNORECASE | re.DOTALL)
    title_match = title_pattern.search(html)
    if not title_match:
        return None

    station_name = title_match.group(1).strip()

    # Extract JavaScript file to find stream ID
    js_pattern = re.compile(r'<script[^>]*src="([^"]*stream\d+\.js)"', re.IGNORECASE)
    js_match = js_pattern.search(html)

    if not js_match:
        return None

    js_file = js_match.group(1)

    # Extract stream ID from JavaScript filename
    stream_id_match = re.search(r'stream(\d+)\.js', js_file)
    if not stream_id_match:
        return None

    stream_id = stream_id_match.group(1)

    # Extract mount point from JavaScript
    js_content = get_page(urljoin(BASE_URL, js_file))
    if not js_content:
        return None

    # Clean up the JavaScript content by removing newlines and extra spaces
    js_content_clean = ' '.join(js_content.split())

    # More specific regex to extract mount point from the AJAX URL
    mount_match = re.search(r'url:\s*[\'"]([^\'"]*mount=([^\'&"]+))[^\'"]*[\'"]', js_content_clean)
    if not mount_match:
        # Try simpler pattern
        mount_match = re.search(r'mount=([^&"]+)', js_content_clean)
        if not mount_match:
            return None

    mount_point = mount_match.group(2) if len(mount_match.groups()) > 1 else mount_match.group(1)

    # Get stream metadata from Icecast
    icecast_url = f"{ICECAST_BASE}/status.xsl?mount={mount_point}"
    icecast_html = get_page(icecast_url)

    stream_data = {
        'station_name': station_name,
        'station_url': station_url,
        'mount_point': mount_point,
        'stream_id': stream_id,
        'stream_urls': {
            'm3u': f"{ICECAST_BASE}{mount_point}.m3u",
            'pls': f"{ICECAST_BASE}{mount_point}.pls",
            'xspf': f"{ICECAST_BASE}{mount_point}.xspf"
        },
        'metadata': {}
    }

    if icecast_html:
        # Extract current song
        song_pattern = re.compile(r'<td[^>]*class="streamdata"[^>]*>(.*?)</td>', re.IGNORECASE | re.DOTALL)
        song_match = song_pattern.search(icecast_html)
        if song_match:
            stream_data['metadata']['current_song'] = song_match.group(1).strip()

        # Extract other metadata from tables
        row_pattern = re.compile(r'<tr[^>]*>(.*?)</tr>', re.IGNORECASE | re.DOTALL)
        for row_match in row_pattern.finditer(icecast_html):
            row_html = row_match.group(1)
            cell_pattern = re.compile(r'<td[^>]*>(.*?)</td>', re.IGNORECASE | re.DOTALL)
            cells = cell_pattern.findall(row_html)
            if len(cells) == 2:
                key = cells[0].strip()
                value = cells[1].strip()
                if key and value:
                    stream_data['metadata'][key.lower().replace(' ', '_')] = value

    return stream_data

def main():
    """Main scraping function"""
    print("Starting Radcap.ru stream scraper...")

    all_streams = []

    # Get all genre links
    genre_links = extract_genre_links()

    # Process each genre
    for i, genre_link in enumerate(genre_links):
        print(f"\nProcessing genre {i+1}/{len(genre_links)}")

        # Get all station links for this genre
        station_links = extract_station_links(genre_link)

        # Process each station
        for j, station_link in enumerate(station_links):
            if j % 10 == 0:  # Print progress every 10 stations
                print(f"  Processing station {j+1}/{len(station_links)}: {station_link}")
            else:
                print(f"  Processing station {j+1}/{len(station_links)}", end='\r')

            try:
                stream_info = extract_stream_info(station_link)
                if stream_info:
                    all_streams.append(stream_info)
                    if j % 10 == 0:  # Print found stream every 10 stations
                        print(f"    Found stream: {stream_info['station_name']}")
            except Exception as e:
                if j % 10 == 0:  # Only print errors occasionally to avoid clutter
                    print(f"    Error processing {station_link}: {e}")

            # Be polite with delays
            time.sleep(0.3)  # Reduced delay for faster scraping

    # Save to JSON file
    output_file = "radcap.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(all_streams, f, ensure_ascii=False, indent=2)

    print(f"\nScraping complete! Found {len(all_streams)} streams.")
    print(f"Results saved to {output_file}")

    return all_streams

if __name__ == "__main__":
    main()
