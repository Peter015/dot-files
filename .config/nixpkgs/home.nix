{ config, pkgs, ... }:

{
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "peterz";
    home.homeDirectory = "/home/peterz";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
   home.stateVersion = "22.05";

    programs.fish = {
        enable = true;

        interactiveShellInit = "starship init fish | source";

        shellAliases = {
            # exa not ls
            ll = "exa -al --color=always --group-directories-first";
            la = "exa -a --color=always --group-directories-first";
            ls = "exa -l --color=always --group-directories-first";
            lt = "exa -aT --color=always --group-directories-first";

            # ripgrep and not grep
            grep = "rg";

            # bat not cat
            cat = "bat";

            #Improve SBCL Repl
            sbcl = "rlwrap -c sbcl";
        };
    };


    programs.emacs = {
        enable = true;
        extraPackages = (
            epkgs: (with epkgs; [
                nix-mode
                tree-sitter
                tree-sitter-langs
                eglot
                neotree
                use-package
                all-the-icons
                slime
                sublime-themes
                color-theme-solarized
                modus-themes
                tide
                company
                flycheck
                monokai-pro-theme
                tide
                web-mode
                paredit
                smartparens
                rainbow-delimiters
                pdf-tools
            ])
        );
    };

    programs.git = {
        enable = true;
        userName = "Peter Zimmermann";
        userEmail = "pzimm30@gmail.com";
    };

    # desktop entries
    xdg.desktopEntries = {
        pcmanfm = {
            type = "Application";
            name = "File Manager PCManFM";
            genericName = "File Manager";
            exec = "pcmanfm %U";
            icon = ~/Pictures/desktop-icons/png-clipart-next-folders-icon-blank-blue-folder-icon.png;
            categories = [ "System" "FileTools" "FileManager" "Utility" "Core" "GTK" ];
            startupNotify = true;
            terminal = false;
            mimeType = [ "inode/directory" ];
        };
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
}
