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
        plugins = [
            {
                name = "z";
                src = pkgs.fetchFromGitHub {
                    owner = "jethrokuan";
                    repo = "z";
                    rev = "e0e1b9dfdba362f8ab1ae8c1afc7ccf62b89f7eb";
                    sha256 = "0dbnir6jbwjpjalz14snzd3cgdysgcs3raznsijd6savad3qhijc";
                };
            }
            
            # { name = "grc"; src = pkgs.fishPlugins.grc; }
            { name = "pure"; src = pkgs.fishPlugins.pure; }
        ];
        
        interactiveShellInit = "starship init fish | source";
        
        shellAliases = {
            
        };     
    };    
    
    
    
    programs.emacs = {
        enable = true;
        # extraPackages = (
            # epkgs: (with epkgs; [
                # nix-mode
                # magit
                # tree-sitter
                # tree-sitter-langs
                # paredit
                # tron-legacy-theme
                # eglot
                # merlin
                # rust-mode
            # ])
        # );
    };
    
    programs.git = {
        enable = true;
        userName = "Peter Zimmermann";
        userEmail = "pzimm30@gmail.com";
    };
  
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
}

