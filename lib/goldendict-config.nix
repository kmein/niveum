{
  pkgs,
  path,
}: ''
  <config>
   <paths>
    <path recursive="1">${path}</path>
   </paths>
   <sounddirs/>
   <dictionaryOrder name="" id="0">
    <mutedDictionaries/>
   </dictionaryOrder>
   <inactiveDictionaries name="" id="0">
    <mutedDictionaries/>
   </inactiveDictionaries>
   <groups nextId="1"/>
   <hunspell dictionariesPath=""/>
   <transliteration>
    <enableRussianTransliteration>0</enableRussianTransliteration>
    <enableGermanTransliteration>0</enableGermanTransliteration>
    <enableGreekTransliteration>0</enableGreekTransliteration>
    <enableBelarusianTransliteration>0</enableBelarusianTransliteration>
    <chinese>
     <enable>0</enable>
     <enableSCToTWConversion>1</enableSCToTWConversion>
     <enableSCToHKConversion>1</enableSCToHKConversion>
     <enableTCToSCConversion>1</enableTCToSCConversion>
    </chinese>
    <romaji>
     <enable>0</enable>
     <enableHepburn>1</enableHepburn>
     <enableNihonShiki>0</enableNihonShiki>
     <enableKunreiShiki>0</enableKunreiShiki>
     <enableHiragana>1</enableHiragana>
     <enableKatakana>1</enableKatakana>
    </romaji>
   </transliteration>
   <forvo>
    <enable>0</enable>
    <apiKey></apiKey>
    <languageCodes></languageCodes>
   </forvo>
   <mediawikis>
    <mediawiki enabled="0" name="English Wikipedia" icon="" id="ae6f89aac7151829681b85f035d54e48" url="https://en.wikipedia.org/w"/>
    <mediawiki enabled="0" name="English Wiktionary" icon="" id="affcf9678e7bfe701c9b071f97eccba3" url="https://en.wiktionary.org/w"/>
    <mediawiki enabled="0" name="German Wikipedia" icon="" id="a8a66331a1242ca2aeb0b4aed361c41d" url="https://de.wikipedia.org/w"/>
    <mediawiki enabled="0" name="German Wiktionary" icon="" id="21c64bca5ec10ba17ff19f3066bc962a" url="https://de.wiktionary.org/w"/>
   </mediawikis>
   <websites>
    <website enabled="0" name="Google En-En (Oxford)" icon="" id="b88cb2898e634c6638df618528284c2d" url="https://www.google.com/search?q=define:%GDWORD%&amp;hl=en" inside_iframe="1"/>
    <website enabled="0" name="Urban Dictionary" icon="" id="f376365a0de651fd7505e7e5e683aa45" url="https://www.urbandictionary.com/define.php?term=%GDWORD%" inside_iframe="1"/>
    <website enabled="0" name="Multitran (En)" icon="" id="324ca0306187df7511b26d3847f4b07c" url="https://multitran.ru/c/m.exe?CL=1&amp;l1=1&amp;s=%GD1251%" inside_iframe="1"/>
    <website enabled="0" name="Lingvo (En-Ru)" icon="" id="924db471b105299c82892067c0f10787" url="http://lingvopro.abbyyonline.com/en/Search/en-ru/%GDWORD%" inside_iframe="1"/>
    <website enabled="0" name="Michaelis (Pt-En)" icon="" id="087a6d65615fb047f4c80eef0a9465db" url="http://michaelis.uol.com.br/moderno/ingles/index.php?lingua=portugues-ingles&amp;palavra=%GDISO1%" inside_iframe="1"/>
   </websites>
   <dictservers/>
   <programs>
    <program enabled="0" name="Espeak" icon="" id="2cf8b3a60f27e1ac812de0b57c148340" commandLine="${pkgs.espeak}/bin/espeak %GDWORD%" type="0"/>
    <program enabled="0" name="Manpages" icon="" id="4f898f7582596cea518c6b0bfdceb8b3" commandLine="${pkgs.man_db}/bin/man -a --html=/bin/cat %GDWORD%" type="2"/>
   </programs>
   <voiceEngines/>
   <mutedDictionaries/>
   <popupMutedDictionaries>
    <mutedDictionary>ae6f89aac7151829681b85f035d54e48</mutedDictionary>
   </popupMutedDictionaries>
   <preferences>
    <interfaceLanguage></interfaceLanguage>
    <helpLanguage></helpLanguage>
    <displayStyle>modern</displayStyle>
    <newTabsOpenAfterCurrentOne>0</newTabsOpenAfterCurrentOne>
    <newTabsOpenInBackground>1</newTabsOpenInBackground>
    <hideSingleTab>0</hideSingleTab>
    <mruTabOrder>0</mruTabOrder>
    <hideMenubar>0</hideMenubar>
    <enableTrayIcon>1</enableTrayIcon>
    <startToTray>1</startToTray>
    <closeToTray>1</closeToTray>
    <autoStart>0</autoStart>
    <doubleClickTranslates>1</doubleClickTranslates>
    <selectWordBySingleClick>0</selectWordBySingleClick>
    <escKeyHidesMainWindow>0</escKeyHidesMainWindow>
    <zoomFactor>1</zoomFactor>
    <helpZoomFactor>1</helpZoomFactor>
    <wordsZoomLevel>0</wordsZoomLevel>
    <enableMainWindowHotkey>1</enableMainWindowHotkey>
    <mainWindowHotkey>Ctrl+F11, Ctrl+F11</mainWindowHotkey>
    <enableClipboardHotkey>1</enableClipboardHotkey>
    <clipboardHotkey>Ctrl+C, Ctrl+C</clipboardHotkey>
    <enableScanPopup>1</enableScanPopup>
    <startWithScanPopupOn>0</startWithScanPopupOn>
    <enableScanPopupModifiers>0</enableScanPopupModifiers>
    <scanPopupModifiers>0</scanPopupModifiers>
    <scanPopupAltMode>0</scanPopupAltMode>
    <scanPopupAltModeSecs>3</scanPopupAltModeSecs>
    <ignoreOwnClipboardChanges>0</ignoreOwnClipboardChanges>
    <scanToMainWindow>0</scanToMainWindow>
    <ignoreDiacritics>0</ignoreDiacritics>
    <showScanFlag>0</showScanFlag>
    <scanPopupUseUIAutomation>1</scanPopupUseUIAutomation>
    <scanPopupUseIAccessibleEx>1</scanPopupUseIAccessibleEx>
    <scanPopupUseGDMessage>1</scanPopupUseGDMessage>
    <scanPopupUnpinnedWindowFlags>0</scanPopupUnpinnedWindowFlags>
    <scanPopupUnpinnedBypassWMHint>0</scanPopupUnpinnedBypassWMHint>
    <pronounceOnLoadMain>0</pronounceOnLoadMain>
    <pronounceOnLoadPopup>0</pronounceOnLoadPopup>
    <useInternalPlayer>1</useInternalPlayer>
    <internalPlayerBackend>FFmpeg+libao</internalPlayerBackend>
    <audioPlaybackProgram>mplayer</audioPlaybackProgram>
    <alwaysOnTop>1</alwaysOnTop>
    <searchInDock>1</searchInDock>
    <historyStoreInterval>0</historyStoreInterval>
    <favoritesStoreInterval>0</favoritesStoreInterval>
    <confirmFavoritesDeletion>1</confirmFavoritesDeletion>
    <proxyserver enabled="0" useSystemProxy="0">
     <type>0</type>
     <host></host>
     <port>3128</port>
     <user></user>
     <password></password>
     <systemProxyUser></systemProxyUser>
     <systemProxyPassword></systemProxyPassword>
    </proxyserver>
    <disallowContentFromOtherSites>0</disallowContentFromOtherSites>
    <enableWebPlugins>0</enableWebPlugins>
    <hideGoldenDictHeader>0</hideGoldenDictHeader>
    <maxNetworkCacheSize>50</maxNetworkCacheSize>
    <clearNetworkCacheOnExit>1</clearNetworkCacheOnExit>
    <maxStringsInHistory>500</maxStringsInHistory>
    <storeHistory>1</storeHistory>
    <alwaysExpandOptionalParts>0</alwaysExpandOptionalParts>
    <addonStyle></addonStyle>
    <collapseBigArticles>0</collapseBigArticles>
    <articleSizeLimit>2000</articleSizeLimit>
    <limitInputPhraseLength>0</limitInputPhraseLength>
    <inputPhraseLengthLimit>1000</inputPhraseLengthLimit>
    <maxDictionaryRefsInContextMenu>20</maxDictionaryRefsInContextMenu>
    <trackClipboardChanges>0</trackClipboardChanges>
    <synonymSearchEnabled>1</synonymSearchEnabled>
    <fullTextSearch>
     <searchMode>0</searchMode>
     <matchCase>0</matchCase>
     <maxArticlesPerDictionary>100</maxArticlesPerDictionary>
     <maxDistanceBetweenWords>2</maxDistanceBetweenWords>
     <useMaxArticlesPerDictionary>0</useMaxArticlesPerDictionary>
     <useMaxDistanceBetweenWords>1</useMaxDistanceBetweenWords>
     <dialogGeometry></dialogGeometry>
     <disabledTypes></disabledTypes>
     <enabled>1</enabled>
     <ignoreWordsOrder>0</ignoreWordsOrder>
     <ignoreDiacritics>0</ignoreDiacritics>
     <maxDictionarySize>0</maxDictionarySize>
    </fullTextSearch>
   </preferences>
   <lastMainGroupId>0</lastMainGroupId>
   <lastPopupGroupId>0</lastPopupGroupId>
   <popupWindowState>AAAA/wAAAAH9AAAAAAAAAg0AAAGTAAAABAAAAAQAAAAIAAAACPwAAAABAAAAAQAAAAEAAAAaAGQAaQBjAHQAaQBvAG4AYQByAHkAQgBhAHIDAAAAAP////8AAAAAAAAAAA==</popupWindowState>
   <popupWindowGeometry>AdnQywADAAAAAAC6AAABEgAAAuYAAAKkAAAAugAAARIAAALmAAACpAAAAAAAAAAABVYAAAC6AAABEgAAAuYAAAKk</popupWindowGeometry>
   <pinPopupWindow>0</pinPopupWindow>
   <popupWindowAlwaysOnTop>0</popupWindowAlwaysOnTop>
   <mainWindowState>AAAA/wAAAAH9AAAAAgAAAAAAAADMAAAC0PwCAAAAAfsAAAAUAHMAZQBhAHIAYwBoAFAAYQBuAGUBAAAAFAAAAtAAAAB9AP///wAAAAEAAADMAAAC0PwCAAAAA/sAAAASAGQAaQBjAHQAcwBQAGEAbgBlAQAAABQAAAFvAAAAYQD////7AAAAGgBmAGEAdgBvAHIAaQB0AGUAcwBQAGEAbgBlAAAAABQAAALQAAAAYQD////7AAAAFgBoAGkAcwB0AG8AcgB5AFAAYQBuAGUBAAABhAAAAWAAAABhAP///wAAA7QAAALQAAAABAAAAAQAAAAIAAAACPwAAAABAAAAAgAAAAIAAAAUAG4AYQB2AFQAbwBvAGwAYgBhAHIAAAAAAP////8AAAAAAAAAAAAAABoAZABpAGMAdABpAG8AbgBhAHIAeQBCAGEAcgAAAAAA/////wAAAAAAAAAA</mainWindowState>
   <mainWindowGeometry>AdnQywADAAAAAAAEAAAAGAAABVEAAAL7AAAABAAAABgAAAVRAAAC+wAAAAAAAAAABVYAAAAEAAAAGAAABVEAAAL7</mainWindowGeometry>
   <helpWindowGeometry>AdnQywADAAAAAAF3AAAAgwAAA9AAAAJGAAABeAAAAIQAAAPPAAACRQAAAAAAAAAABVYAAAF4AAAAhAAAA88AAAJF</helpWindowGeometry>
   <helpSplitterState>AAAA/wAAAAEAAAACAAABBAAABAAB/////wEAAAABAA==</helpSplitterState>
   <dictInfoGeometry>AdnQywADAAAAAAF1AAAAmgAAA84AAAIrAAABdgAAAJsAAAPNAAACKgAAAAAAAAAABVYAAAF2AAAAmwAAA80AAAIq</dictInfoGeometry>
   <inspectorGeometry></inspectorGeometry>
   <timeForNewReleaseCheck></timeForNewReleaseCheck>
   <skippedRelease></skippedRelease>
   <showingDictBarNames>1</showingDictBarNames>
   <usingSmallIconsInToolbars>1</usingSmallIconsInToolbars>
   <editDictionaryCommandLine></editDictionaryCommandLine>
   <maxPictureWidth>0</maxPictureWidth>
   <maxHeadwordSize>256</maxHeadwordSize>
   <maxHeadwordsToExpand>0</maxHeadwordsToExpand>
   <headwordsDialog>
    <searchMode>0</searchMode>
    <matchCase>0</matchCase>
    <autoApply>0</autoApply>
    <headwordsExportPath></headwordsExportPath>
    <headwordsDialogGeometry></headwordsDialogGeometry>
   </headwordsDialog>
  </config>
''
