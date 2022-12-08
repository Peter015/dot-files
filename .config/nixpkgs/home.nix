{ config, pkgs, ... }:

{
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    # home.username = "peterz";
    # home.homeDirectory = "/home/peterz";

    home = {
      username = "peterz";
      homeDirectory = "/home/peterz";
    };

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

            # dot-files alias
            config = "git --git-dir=/home/peterz/.cfg/ --work-tree=~";
            
            # Element
            msg = "flatpak run im.riot.Riot --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer --ozone-platform=wayland";
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
                all-the-icons
                tide
                flycheck
                smartparens
                rainbow-delimiters
                highlight-indent-guides
                sly
                company
                projectile
                ivy
                counsel
                swiper
                which-key
                magit
                counsel-projectile
                markdown-mode
                ivy-rich
                anaconda-mode
                company-anaconda
                minimap
                darkroom
                racket-mode
                smart-mode-line
                yuck-mode
                rust-mode
            ])
        );
    };

    programs.git = {
        enable = true;
        userName = "Peter Zimmermann";
        userEmail = "pzimm30@gmail.com";
    };
    
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
}
