const { execFileSync } = require('child_process');
execFileSync(`${__dirname}/install-nix`, { stdio: 'inherit' });
