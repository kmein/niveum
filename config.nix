{ config, lib, pkgs, ... }:
let
  uiFont = "Roboto";
  uiFontSize = 10;
  defaultTerminal = "${pkgs.lxqt.qterminal}/bin/qterminal";
  white = "#ffffff";
  black = "#000000";
  lightGray = "#aaaaaa";
  darkGray = "#888888";
  red = "#ff0000";

  spotify_info = pkgs.writeScript "spotify.info" ''
    #!/bin/bash

    STATUS=$(${pkgs.dbus}/bin/dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'PlaybackStatus'|egrep -A 1 "string"|cut -b 26-|cut -d '"' -f 1|egrep -v ^$)

    if [[ "$STATUS" == 'Playing' ]]; then
      printf '\uf1bc  '
      printf '\uf04b'
    elif [[ "$STATUS" == 'Paused' ]]; then
      printf '\uf1bc  '
      printf '\uf04c'
    elif [[ "$STATUS" == 'Stopped' ]]; then
      printf '\uf1bc  '
      printf '\uf04d'
    else
      exit 1
    fi

    printf '  '

    METADATA=$(${pkgs.dbus}/bin/dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata')
    ARTIST=$(echo "$METADATA" | egrep -A 2 "artist" | egrep -v "artist" | egrep -v "array" | cut -b 27- | cut -d '"' -f 1 | egrep -v ^$)
    TITLE=$(echo "$METADATA" | egrep -A 1 "title" | egrep -v "title" | cut -b 44- | cut -d '"' -f 1 | egrep -v ^$)

    printf "%s \u2237 %s" "$ARTIST" "$TITLE"
  '';
  battery_info = pkgs.writeScript "battery.info" ''
    #!/usr/bin/env bash
    cd "/sys/class/power_supply/$BLOCK_INSTANCE/"

    status=$(cat status)
    charge_f=$((100 * $(cat charge_now) / $(cat charge_full)))

    if [[ "$charge_f" -lt 20 ]]; then
      printf '\uf244'
    elif [[ "$charge_f" -lt 40 ]]; then
      printf '\uf243'
    elif [[ "$charge_f" -lt 60 ]]; then
      printf '\uf242'
    elif [[ "$charge_f" -lt 80 ]]; then
      printf '\uf241'
    else
      printf '\uf240'
    fi

    printf '  '

    if [[ "$status" == 'Charging' ]]; then
      printf '\uf106'
    elif [[ "$status" == 'Discharging' ]]; then
      printf '\uf107'
    elif [[ "$status" == 'Full' ]]; then
      printf '\uf0e7'
    else
      printf '[%s]' "$status"
    fi

    printf '  '

    if [[ "$status" != 'Full' ]]; then
      rate_raw=$(($(cat voltage_now) * $(cat power_now)))
      rate=$(bc <<< "scale=1; $rate_raw / 10^12")
      printf '%s\u2009W, ' "$rate"
    fi

    charge_d=$((100 * $(cat charge_now) / $(cat charge_full)))
    printf '%s%%\n' "$charge_d"

    if [[ "$status" == 'Discharging' ]]; then
      if [[ "$charge_d" -lt 10 ]]; then
        printf '\n#E41C28'
      elif [[ "$charge_d" -lt 20 ]]; then
        printf '\n#EEBF13'
      fi
    fi
  '';
  volume_info = pkgs.writeScript "volume.info" ''
    #!/usr/bin/env bash
    if [[ "$BLOCK_BUTTON" == 1 ]]; then
      ${pkgs.pamixer}/bin/pamixer -i 5
    elif [[ "$BLOCK_BUTTON" == 3 ]]; then
      ${pkgs.pamixer}/bin/pamixer -d 5
    elif [[ "$BLOCK_BUTTON" == 2 ]]; then
      ${pkgs.pamixer}/bin/pamixer -t
    fi

    if $(${pkgs.pamixer}/bin/pamixer --get-mute); then
      printf '\uf026  0%%\n\n#EEBF13'
    else
      volume=$(${pkgs.pamixer}/bin/pamixer --get-volume)
      printf '\uf028  %s%%' "$volume"
    fi
  '';
  fancyDate = pkgs.writeScript "fancy_date.py" ''
    #!/usr/bin/env python3
    from datetime import datetime
    now = datetime.now()
    print(now.strftime("%d\u2009{}\u2009%Y ⟨%V⟩").format(chr(0x2160 + (now.month - 1))))
  '';
  i3blocks_conf = pkgs.writeText "i3blocks.conf" ''
    markup=pango
    align=center
    color=${white}

    [spotify]
    command=${spotify_info}
    interval=1

    [separator]

    [volume]
    command=${volume_info}
    min_width= 100%
    interval=once
    signal=3

    [separator]

    [brightness]
    command=printf "%.1f%%" $(${pkgs.xorg.xbacklight}/bin/xbacklight)
    label=
    min_width= 100%
    signal=2
    interval=once

    [separator]

    [cpu_usage]
    command=cut -d' ' -f 1-3 < /proc/loadavg
    label=
    interval=2

    [separator]

    [ram_usage]
    command=free -h | grep "Mem" | awk '{print $3}'
    label=
    interval=2
    align=center

    [separator]

    [battery]
    command=${battery_info}
    #echo "$(cat /sys/class/power_supply/BAT1/capacity)% ($(cat /sys/class/power_supply/BAT1/status))"
    interval=10
    instance=BAT1

    [separator]

    [date]
    #command=date +'%F [%a(%V):%j]'
    command=${fancyDate}
    interval=30
    label=

    [separator]

    [time]
    command=date +'%H:%M'
    interval=30
    label=

    [separator]
  '';
  i3_conf = pkgs.writeText "i3.conf" ''
    set $rofsize 12
    set $mainfont ${uiFont} ${uiFontSize}

    set $accent-color ${darkGray}
    set $bg-color ${black}
    set $border-color ${black}
    set $inactive-bg-color ${black}
    set $inactive-text-color ${darkGray}
    set $active-text-color ${lightGray}
    set $indicator-color ${white}
    set $text-color ${white}
    set $urgent-color ${red}
    set $ws-bg-color ${black}
    set $ws-border-color ${black}
    set $ws-inactive-bg-color ${black}

    set $terminal ${defaultTerminal}
    set $file-manager ${pkgs.gnome3.nautilus}/bin/nautilus
    set $browser ${pkgs.google-chrome}/bin/google-chrome-stable

    set $mod Mod4

    font pango:${uiFont} ${uiFontSize}
    floating_modifier $mod

    hide_edge_borders both
    new_window pixel 1
    new_float  pixel 1

    bindsym $mod+Return    exec $terminal
    bindsym $mod+y       exec $browser
    bindsym $mod+t       exec $file-manager
    bindsym $mod+Shift+q   kill
    bindsym $mod+Left    focus left
    bindsym $mod+Down    focus down
    bindsym $mod+Up      focus up
    bindsym $mod+Right     focus right
    bindsym $mod+p       workspace prev
    bindsym $mod+n       workspace next
    bindsym $mod+Shift+Left  move left
    bindsym $mod+Shift+Down  move down
    bindsym $mod+Shift+Up  move up
    bindsym $mod+Shift+Right move right
    bindsym $mod+h       split h
    bindsym $mod+v       split v
    bindsym $mod+f       fullscreen toggle
    bindsym $mod+s       layout stacking
    bindsym $mod+w       layout tabbed
    bindsym $mod+e       layout toggle split
    bindsym $mod+Shift+z   floating toggle
    bindsym $mod+Shift+c   reload
    bindsym $mod+Shift+r   restart
    bindsym $mod+d       exec ${pkgs.rofi}/bin/rofi -show run

    set $WS1 1
    set $WS2 2
    set $WS3 3
    set $WS4 4
    set $WS5 5
    set $WS6 6
    set $WS7 7
    set $WS8 8
    set $WS9 9
    set $WS10 10
    bindsym $mod+0     workspace $WS10
    bindsym $mod+1     workspace $WS1
    bindsym $mod+2     workspace $WS2
    bindsym $mod+3     workspace $WS3
    bindsym $mod+4     workspace $WS4
    bindsym $mod+5     workspace $WS5
    bindsym $mod+6     workspace $WS6
    bindsym $mod+7     workspace $WS7
    bindsym $mod+8     workspace $WS8
    bindsym $mod+9     workspace $WS9
    bindsym $mod+Shift+0 move container to workspace $WS10
    bindsym $mod+Shift+1 move container to workspace $WS1
    bindsym $mod+Shift+2 move container to workspace $WS2
    bindsym $mod+Shift+3 move container to workspace $WS3
    bindsym $mod+Shift+4 move container to workspace $WS4
    bindsym $mod+Shift+5 move container to workspace $WS5
    bindsym $mod+Shift+6 move container to workspace $WS6
    bindsym $mod+Shift+7 move container to workspace $WS7
    bindsym $mod+Shift+8 move container to workspace $WS8
    bindsym $mod+Shift+9 move container to workspace $WS9

    bindsym XF86AudioLowerVolume  exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5 && pkill -RTMIN+3 i3blocks
    bindsym XF86AudioRaiseVolume  exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5 && pkill -RTMIN+3 i3blocks
    bindsym XF86AudioMute     exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t && pkill -RTMIN+3 i3blocks
    bindsym XF86MonBrightnessUp   exec --no-startup-id ${pkgs.xorg.xbacklight}/bin/xbacklight + 10 && pkill -RTMIN+2 i3blocks
    bindsym XF86MonBrightnessDown exec --no-startup-id ${pkgs.xorg.xbacklight}/bin/xbacklight - 10 && pkill -RTMIN+2 i3blocks

    mode "  " {
      bindsym Left   resize shrink width 10 px or 10 ppt
      bindsym Down   resize grow height 10 px or 10 ppt
      bindsym Up   resize shrink height 10 px or 10 ppt
      bindsym Right  resize grow width 10 px or 10 ppt
      bindsym Escape mode "default"
    }
    bindsym $mod+r mode "  "

    client.focused      $accent-color $bg-color      $text-color      $indicator-color $accent-color
    client.focused_inactive $border-color $inactive-bg-color $inactive-text-color $indicator-color $border-color
    client.unfocused    $border-color $inactive-bg-color $inactive-text-color $indicator-color $border-color
    client.urgent       $urgent-color $bg-color      $text-color      $indicator-color $urgent-color

    bar {
      status_command "${pkgs.i3blocks}/bin/i3blocks -c ${i3blocks_conf}"
      position top

      font pango:$font,FontAwesome $size
      separator_symbol " // "
      colors {
        separator $inactive-text-color
        background $inactive-bg-color
        statusline $inactive-text-color

        focused_workspace  $ws-border-color $ws-bg-color      $text-color
        active_workspace   $ws-border-color $ws-bg-color      $active-text-color
        inactive_workspace $ws-border-color $ws-inactive-bg-color $inactive-text-color
        urgent_workspace   $ws-border-color $ws-bg-color      $urgent-color
      }
    }

    exec --no-startup-id ${pkgs.networkmanagerapplet}/bin/nm-applet
  '';
  vim_conf = pkgs.writeText "vim.conf" ''
    let mapleader=","
    let maplocalleader=";"

    filetype off

    set packpath^=~/.vim
    packadd minpac
    call minpac#init()
    call minpac#add('LnL7/vim-nix')
    call minpac#add('MarcWeber/vim-addon-mw-utils')
    call minpac#add('Shougo/neocomplete.vim')
    call minpac#add('Shougo/vimproc.vim')
    call minpac#add('airblade/vim-gitgutter')
    call minpac#add('bling/vim-bufferline')
    call minpac#add('ctrlpvim/ctrlp.vim')
    call minpac#add('dhruvasagar/vim-table-mode')
    call minpac#add('fsharp/vim-fsharp')
    call minpac#add('garbas/vim-snipmate')
    call minpac#add('gerw/vim-latex-suite')
    call minpac#add('godlygeek/tabular')
    call minpac#add('idris-hackers/idris-vim')
    call minpac#add('k-takata/minpac', {'type':'opt'})
    call minpac#add('kien/rainbow_parentheses.vim')
    call minpac#add('leafgarland/typescript-vim')
    call minpac#add('mattn/emmet-vim')
    call minpac#add('mhinz/vim-startify')
    call minpac#add('mxw/vim-jsx')
    call minpac#add('pangloss/vim-javascript')
    call minpac#add('raichoo/purescript-vim')
    call minpac#add('rust-lang/rust.vim')
    call minpac#add('scrooloose/nerdcommenter')
    call minpac#add('scrooloose/nerdtree')
    call minpac#add('scrooloose/syntastic')
    call minpac#add('tomtom/tlib_vim')
    call minpac#add('toyamarinyon/vim-swift')
    call minpac#add('tpope/vim-fugitive')
    call minpac#add('tpope/vim-git')
    call minpac#add('tpope/vim-repeat')
    call minpac#add('tpope/vim-speeddating')
    call minpac#add('tpope/vim-surround')
    call minpac#add('vim-airline/vim-airline')
    call minpac#add('vim-airline/vim-airline-themes')
    call minpac#add('vim-pandoc/vim-pandoc-syntax')
    call minpac#add('vim-scripts/Gundo')
    call minpac#add('vim-scripts/SuperTab')

    let g:vimtex_disable_version_warning = 1

    let g:airline#extensions#tabline#close_symbol = 'X'
    let g:airline#extensions#tabline#enabled = 0
    let g:airline#extensions#tabline#left_alt_sep = ''
    let g:airline#extensions#tabline#left_sep = ''
    let g:airline#extensions#tabline#right_alt_sep = ''
    let g:airline#extensions#tabline#right_sep = ''
    let g:airline#extensions#tabline#show_close_button = 1
    let g:airline#extensions#tabline#show_tab_type = 0
    let g:airline#extensions#tabline#tab_min_count = 2
    let g:airline#extensions#tabline#tab_nr_type = 0
    let g:airline#extensions#tmuxline#enabled = 0
    let g:airline#extensions#wordcount#enabled = 1
    let g:airline_left_alt_sep = ''
    let g:airline_left_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_section_z = '%{line(".") + 1}/%{line("$")}'
    let g:airline_theme='simple'

    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 0
    let g:syntastic_check_on_open = 0
    let g:syntastic_check_on_wq = 0

    nnoremap <F5> :GundoToggle<CR>

    map ,s :SyntasticToggleMode<CR>
    map <silent> tw :GhcModTypeInsert<CR>
    map <silent> ts :GhcModSplitFunCase<CR>
    map <silent> tq :GhcModType<CR>
    map <silent> te :GhcModTypeClear<CR>
    let g:haskellmode_completion_ghc = 1

    let g:SuperTabDefaultCompletionType = '<c-n>'  " '<c-x><c-o>'

    if has("gui_running")
      imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
    else " no gui
      if has("unix")
        inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
      endif
    endif

    vmap a= :Tabularize /=<CR>
    vmap a; :Tabularize /::<CR>
    vmap a- :Tabularize /-><CR>


    """ {{{ Set commands
    syntax on
    colorscheme delek

    filetype plugin indent on

    set path=$PWD/**
    set wildmenu
    set shortmess+=I
    set noshowcmd
    set nocompatible
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set number
    set nowb
    set noswapfile
    set mouse=a
    set ruler
    set ignorecase smartcase
    set showmatch
    set encoding=utf8
    set ffs=unix,dos,mac
    set autoindent smartindent
    set nowrap
    set laststatus=2
    set noshowmode " vim-airline is installed, no need for that
    set linespace=0
    set nohlsearch
    set clipboard=unnamedplus,autoselect
    set completeopt=menuone,menu,longest
    set wildmode=longest,list,full
    set nopaste
    set title
    set titleold=""
    set titlestring=VIM:\ %F
    set list
    set listchars=tab:▸\ ,extends:❯,precedes:❮
    set showbreak=↪
    set norelativenumber
    set foldlevelstart=30

    let g:netrw_banner=0
    let g:netrw_browse_split=4
    let g:netrw_altv=1 " open splits to the right
    let g:netrw_liststyle=3 " tree view
    let g:netrw_list_hide=netrw_gitignore#Hide()
    let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

    call matchadd('colorcolumn', '\%101v', 100)
    highlight folded ctermbg=black
    highlight colorcolumn ctermbg=red
    highlight matchparen cterm=bold ctermbg=black ctermfg=white


    if exists("+undofile")
      " undofile - This allows you to use undos after exiting and restarting
      " This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
      " :help undo-persistence
      " This is only present in 7.3+
      if isdirectory($HOME . '/.vim/undo') == 0
      :silent !mkdir -p ~/.vim/undo > /dev/null 2>&1
      endif
      set undodir=./.vim-undo//
      set undodir+=~/.vim/undo//
      set undofile
    endif
    """ }}}

    " map <leader>m :%s/^M//<cr>

    " improve up/down navigation in wrapped lines
    nnoremap j gj
    nnoremap k gk
    nnoremap 0 ^

    " greek input
    nnoremap η h
    nnoremap ξ j
    nnoremap κ k
    nnoremap λ l
    nnoremap ι i
    " nmap t o<ESC>k
    " nmap T O<ESC>j

    function! AdjustTabbingTo2()
      set noexpandtab tabstop=4
      retab!
      set expandtab tabstop=2
      retab!
    endfunction

    " nmap <leader>t <SID>AdjustTabbingTo2()

    nmap <C-k> ddkP
    nmap <C-j> ddp

    vmap <C-k> xkP`[V`]
    vmap <C-j> xp`[V`]
    """ }}}

    """ {{{ Practicalities
    function! <SID>StripTrailingWhitespaces()
      let _s=@/
      let l=line(".")
      let c=col(".")

      %s/\s\+$//e

      let @/=_s
      call cursor(l,c)
    endfunction

    if has("autocmd")
      autocmd filetype markdown,text set formatprg=par\ -w80
      autocmd filetype haskell set formatprg=hindent
      autocmd filetype markdown,text set textwidth=80
      autocmd filetype markdown,text set formatoptions+=t
      autocmd bufreadpost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
          \ exe "normal! g`\"" |
        \ endif
      autocmd bufwritepre * :call <SID>StripTrailingWhitespaces()
      " autocmd bufnewfile,bufread *.md set filetype=markdown
      autocmd bufnewfile,bufread *.asm set filetype=nasm
      autocmd bufnewfile,bufread *.bf set filetype=brainfuck
      autocmd bufnewfile,bufread *.do set filetype=sh
      autocmd bufnewfile,bufread config set filetype=conf
      autocmd bufnewfile,bufread *.conf set filetype=conf
      autocmd bufnewfile,bufread *.4th set filetype=forth
      autocmd bufnewfile,bufread *.tex set filetype=tex
      autocmd bufnewfile,bufread *.c set keywordprg=man\ 3
      autocmd bufnewfile,bufread *.h set keywordprg=man\ 3

      autocmd filetype make setlocal noexpandtab

      autocmd! bufnewfile,buffilepre,bufread *.md set filetype=markdown ".pandoc
      autocmd bufreadpre * setlocal foldmethod=indent
      autocmd bufwinenter * if &fdm == 'indent' | setlocal foldmethod=manual | endif
    endif

    nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
    vnoremap <Space> zf

    highlight TrailSpace ctermbg=red guibg=darkred
    match TrailSpace /\s\+$/

    command! RandomLine execute 'normal! '.(system('/bin/bash -c "echo -n $RANDOM"') % line('$')).'G'


    function! s:DiffWithSaved()
      let filetype=&ft
      diffthis
      vnew | r # | normal! 1Gdd
      diffthis
      execute "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
    endfunction
    command! DiffSaved call s:DiffWithSaved()
  '';
in {
  imports = [ "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos" ];
  home-manager.users.my_username = { };

  nixpkgs.config.allowUnfree = true;

  security.sudo.enable = true;

  fonts.fonts = with pkgs; [ roboto font-awesome-ttf fira-code eb-garamond lmodern ];
  environment.systemPackages = with pkgs; [
    stack python3 rustup nasm gcc tinycc nodejs ocaml fsharp clojure racket-minimal jo haskellPackages.hasktags perl ruby scala swiProlog jdk haskellPackages.idris ctags

    ffmpeg mpv youtubeDL
    inkscape imagemagick
    zathura calibre
    google-play-music-desktop-player spotify gnome3.gnome-music audacity

    par haskellPackages.pandoc biber

    google-chrome firefox lynx w3m

    lxappearance
    lxqt.qterminal kitty
    arc-icon-theme arc-kde-theme arc-theme breeze-qt5
    xorg.xbacklight pamixer
    gnome3.nautilus
    kdeconnect
    vim
    git
    wget
    htop
    zip unzip
    ripgrep
    tmux
    whois
    tree
    texlive.combined.scheme-full
    rlwrap
    lua
    libreoffice-fresh
    pmount
    gnumake
    franz
    grive2
    geogebra gnuplot maxima
  ];

  /*networking.hostname = "scardanelli";*/
  networking.networkmanager.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "de, gr, ru";
  services.xserver.xkbVariant = "T3, polytonic, phonetic";
  services.xserver.xkbOptions = "terminate:ctrl_alt_bksp, grp:alt_space_toggle";
  i18n.consoleUseXkbConfig = true;
  services.xserver.libinput.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.gtk.clock-format = "%F";
  services.xserver.displayManager.lightdm.greeters.gtk.iconTheme = { package = pkgs.arc-icon-theme; name = "Arc"; };
  services.xserver.displayManager.lightdm.greeters.gtk.theme = { package = pkgs.arc-theme; name = "Arc-Dark"; };
  services.xserver.windowManager.default = "i3";
  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.i3.extraPackages = with pkgs; [ i3blocks networkmanagerapplet ];
  services.xserver.windowManager.i3.configFile = i3_conf;
  environment.etc.i3blocks_conf.source = i3blocks_conf;
  environment.etc.vimrc.source = vim_conf;

  programs.home-manager.enable = true;
  programs.rofi = {
    enable = true;
    lines = 4;
    font = uiFont + " " + uiFontSize;
    terminal = defaultTerminal;
    theme = "Arc-Dark";
  };

  services.compton = { enable = true; fade = true; shadow = true; fadeDelta = 2; };
  services.openssh.enable = true;

  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  programs.vim.defaultEditor = true;
  programs.zsh.enable = true;
  programs.zsh.autosuggestions.enable = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.syntaxHighlighting.highlighters = [ "main" "brackets" "pattern" "cursor" "root" "line" ];
  programs.zsh.ohMyZsh.enable = true;
  programs.zsh.ohMyZsh.plugins = [ "common-aliases" "git" "git-extras" "history" "jsontools" ];
  programs.zsh.ohMyZsh.theme = "muse";

  users.users.kfm = {
    createHome = true;
    description = "Kierán Meinhardt";
    extraGroups = [ "wheel" "networkmanager" ];
    group = "users";
    home = "/home/kfm";
    shell = pkgs.zsh;
    uid = 1234;
    password = "kfm";
  };
}
