{
	description = "Reproducible R environment with Nix flake";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem (system:
		let
			pkgs = import nixpkgs { inherit system; };
			r-packages = with pkgs.rPackages; [
				Rtsne
				broom
				ggsignif
				ggsurvfit
				gtsummary
				janitor
				missForest
				rmarkdown
				survival
				tidyverse
			];
			r-with-packages = pkgs.rWrapper.override { packages = r-packages; };
			rstudio-with-packages = pkgs.rstudioWrapper.override { packages = r-packages; };
			render-cmd = pkgs.writeShellApplication {
				name = "render";
				runtimeInputs = [ r-with-packages ];
				text = ''
					${r-with-packages}/bin/Rscript -e "rmarkdown::render('tcd-als-immunology.Rmd')"
				'';
			};
			rstudio-cmd = pkgs.writeShellApplication {
				name = "rstudio";
				runtimeInputs = [ rstudio-with-packages ];
				text = ''
					${rstudio-with-packages}/bin/rstudio tcd-als-immunology.Rmd
				'';
			};
		in {
			devShells.default = pkgs.mkShell {
				packages = [ r-with-packages rstudio-with-packages ];
			};
			apps.rstudio = {
				type = "app";
				program = "${rstudio-cmd}/bin/rstudio";
			};
			apps.render = {
				type = "app";
				program = "${render-cmd}/bin/render";
			};
		});
}