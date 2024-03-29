# Validate that go is installed
if ! (command -v go > /dev/null); then
   echo >&2 "Golang must be installed.  Aborting.";
   exit 1;
fi

# REPL!
go install github.com/x-motemen/gore/cmd/gore@latest

# Autocompletion
go install github.com/stamblerre/gocode@latest

# Documentation
go install golang.org/x/tools/cmd/godoc@latest

# Add/Removed Necessary Imports
go install golang.org/x/tools/cmd/goimports@latest

# Type-Safe Renaming of Go identifiers
go install golang.org/x/tools/cmd/gorename@latest

# LSP language server
go install golang.org/x/tools/gopls@latest

# Go linter
go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.43.0

# Asks questions about your Gocode
go install golang.org/x/tools/cmd/guru@latest

# Generate tests based off of the func you're on
go install github.com/cweill/gotests/gotests@latest

# Add `json` or `bson` to structs easily
go install github.com/fatih/gomodifytags@latest

# Find symbol information in Go source
go install github.com/rogpeppe/godef@latest
