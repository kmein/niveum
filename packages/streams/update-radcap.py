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

# Set to True to print raw JS content when Icecast base extraction fails,
# so you can see exactly what the JS looks like and fix the regex.
DEBUG = False

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
}

def get_page(url, timeout=15):
    """Fetch a webpage with retry logic and timeout"""
    try:
        req = urllib.request.Request(url, headers=headers)
        with urllib.request.urlopen(req, timeout=timeout) as response:
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

def extract_icecast_base(js_content_clean):
    """
    Extract the Icecast server base URL (scheme + host + port) from the
    station's JavaScript file.

    Radcap uses protocol-relative URLs like //79.111.14.76:8000/status.xsl?mount=/foo
    so we must match both https?:// and bare //.

    Tries patterns in order of specificity:
    1. Protocol-relative or full URL containing ?mount=
    2. Protocol-relative or full URL with a numeric port
    3. Separate host + port JS variables
    4. Bare quoted IP:port string
    """
    from urllib.parse import urlparse

    # Matches either  'http://host:port/...'  or  '//host:port/...'
    url_prefix = r'(?:https?:)?//'

    # 1. URL that includes ?mount= — most specific
    m = re.search(r'["\'](' + url_prefix + r'[^"\']+\?mount=[^"\']+)["\']', js_content_clean)
    if m:
        url_str = m.group(1)
        # urlparse needs a scheme; add http if protocol-relative
        if url_str.startswith('//'):
            url_str = 'http:' + url_str
        parsed = urlparse(url_str)
        return f"http://{parsed.netloc}"

    # 2. Any URL with a numeric port (Icecast always has one)
    m = re.search(r'["\'](' + url_prefix + r'[\w.\-]+:\d+)', js_content_clean)
    if m:
        url_str = m.group(1)
        if url_str.startswith('//'):
            url_str = 'http:' + url_str
        parsed = urlparse(url_str)
        return f"http://{parsed.netloc}"

    # 3. Separate host/server + port JS variables e.g. host: '1.2.3.4', port: 8000
    host_m = re.search(r'(?:host|server)\s*[=:]\s*["\']([^"\']+)["\']', js_content_clean, re.IGNORECASE)
    port_m = re.search(r'\bport\s*[=:]\s*["\']?(\d+)["\']?', js_content_clean, re.IGNORECASE)
    if host_m and port_m:
        return f"http://{host_m.group(1)}:{port_m.group(1)}"

    # 4. Bare quoted IP:port string anywhere in the JS
    m = re.search(r'["\'](\d{1,3}(?:\.\d{1,3}){3}:\d+)["\']', js_content_clean)
    if m:
        return f"http://{m.group(1)}"

    return None

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

    # --- FIX: extract the Icecast base URL from the JS, not a hardcoded global ---
    icecast_base = extract_icecast_base(js_content_clean)
    if not icecast_base:
        if DEBUG:
            print(f"  [DEBUG] Raw JS for {station_url}:")
            print(f"  [DEBUG] JS URL: {urljoin(BASE_URL, js_file)}")
            print(f"  [DEBUG] JS content (cleaned):\n{js_content_clean}\n")
        print(f"  Warning: could not determine Icecast base for {station_url}, skipping")
        return None

    return {
        'station_name': station_name,
        'station_url': station_url,
        'mount_point': mount_point,
        'stream_id': stream_id,
        'icecast_base': icecast_base,
        'stream_urls': {
            'm3u': f"{icecast_base}{mount_point}.m3u",
            'pls': f"{icecast_base}{mount_point}.pls",
            'xspf': f"{icecast_base}{mount_point}.xspf"
        },
    }

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
            try:
                stream_info = extract_stream_info(station_link)
                if stream_info:
                    all_streams.append(stream_info)
                    print(f"  [{j+1}/{len(station_links)}] Found: {stream_info['station_name']} ({stream_info['icecast_base']})")
                else:
                    print(f"  [{j+1}/{len(station_links)}] Skipped: {station_link}")
            except Exception as e:
                print(f"  [{j+1}/{len(station_links)}] Error: {station_link}: {e}")

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
