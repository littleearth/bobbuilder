# Delphi CI/CD Example

This folder contains example CI/CD configurations for building Delphi applications using BOBBuilder in continuous integration environments.

> **Important Note:** These CI/CD configuration files are examples only. They will NOT automatically trigger when located in this subfolder. To use them in your project, you must copy them to the correct locations in your repository root (see [Installation](#installation) below).

## Contents

This example includes configurations for:

- **GitLab CI/CD** (`.gitlab-ci.yml`)
- **GitHub Actions** (`.github/workflows/`)

## Installation

To use these CI/CD configurations in your own project, you need to copy them to the correct locations:

### For GitLab CI/CD

Copy the GitLab CI configuration to your repository root:

```bash
# From your repository root
cp examples/cicd/.gitlab-ci.yml .gitlab-ci.yml
```

The file must be named `.gitlab-ci.yml` and located at the root of your repository.

### For GitHub Actions

Create the workflows directory structure and copy the workflow files:

```bash
# From your repository root
mkdir -p .github/workflows
cp examples/cicd/.github/workflows/staging.yml .github/workflows/staging.yml
cp examples/cicd/.github/workflows/production.yml .github/workflows/production.yml
```

GitHub Actions requires workflow files to be in `.github/workflows/` at the repository root.

### Copy and Customize Builder Configuration

You'll also need to copy and customize the builder configuration:

```bash
# Copy the builder file to your project location
cp examples/cicd/delphi-ci-test.builder your-project.builder

# Update the CI variable to point to your builder file
# For GitLab: Update CI_BUILDER_FILENAME variable
# For GitHub: Update BUILDER_FILENAME in workflow files
```

## GitLab CI/CD

The `.gitlab-ci.yml` file demonstrates a complete CI/CD pipeline with the following stages:

### Stages

1. **Init** - Auto-increment build number for staging builds
2. **Build** - Compile with BOBBuilder for staging and production
3. **Upload** - Upload artifacts to GitLab Package Registry
4. **Release** - Create GitLab releases with download links

### Jobs

- `auto_increment-staging` - Increments the build number stored in GitLab variables
- `test` - Builds the staging environment configuration
- `production` - Builds the production configuration (triggered on tags)
- `upload` - Uploads installers to GitLab Package Registry
- `release` - Creates a GitLab release with links to installers

### Required Variables

Configure these in GitLab Settings → CI/CD → Variables:

- `CI_BUILDER_FILENAME` - Name of the .builder configuration file
- `CI_VER_BUILD_NUMBER` - Initial build number (auto-incremented)
- `CI_PIPELINE_IID_TOKEN` - GitLab API token with API access

## GitHub Actions

The `.github/workflows/` folder contains two workflow files:

### Workflows

1. **Staging Build** (`staging.yml`)
   - Triggered on push to `main` or `master` branches
   - Auto-increments build number using GitHub repository variables
   - Builds with BOBBuilder using Staging configuration
   - Uploads artifacts with 5-day retention

2. **Production Release** (`production.yml`)
   - Triggered on tag creation (e.g., `v1.0.0` or `1.0.0`)
   - Builds with BOBBuilder using Production configuration
   - Creates GitHub Release with installer files attached

### Setup Instructions

1. **Create Repository Variables** (Settings → Secrets and variables → Actions → Variables):
   - `BUILD_NUMBER` - Initial build number (e.g., `1`)

2. **Create Repository Secrets** (Settings → Secrets and variables → Actions → Secrets):
   - `PAT_TOKEN` - Personal Access Token with `repo` and `workflow` scopes (for updating variables)

3. **Create Environments** (Settings → Environments):
   - Create `staging` environment
   - Create `production` environment
   - Configure protection rules and approvals as needed

4. **BOBBuilder Installation**:
   - Ensure `bobbuilder.exe` is in the system PATH on your runner
   - Or modify the workflows to install/download BOBBuilder before building

### Triggering Builds

**Staging Build:**
```bash
git add .
git commit -m "Your changes"
git push origin main
```

**Production Release:**
```bash
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
```

Or create a release through GitHub's web interface.

## Project Structure

```
examples/cicd/
├── .github/
│   └── workflows/
│       ├── staging.yml          # GitHub Actions staging workflow
│       └── production.yml       # GitHub Actions production workflow
├── .gitlab-ci.yml              # GitLab CI/CD configuration
├── delphi-ci-test.builder      # BOBBuilder configuration
├── installer/                  # InnoSetup installer scripts
├── resources/                  # Common resources and libraries
└── source/                     # Delphi source code
    ├── client/                 # Main application
    ├── common/                 # Shared units
    └── tests/                  # DUnit tests
```

## BOBBuilder Configuration

The `delphi-ci-test.builder` file defines:

- Project information and versioning
- Build configurations (Development, Staging, Production)
- Project groups to build
- Test projects to run
- Installer scripts (InnoSetup, WiX, code signing)
- Pre/post build scripts
- Build complete scripts for deployment

## Features Demonstrated

### Automated Versioning
- Auto-incrementing build numbers
- Version stamping via resource templates
- Environment-specific version strings

### Multi-Platform Builds
- Win32 and Win64 configurations
- Platform-specific output paths

### Testing
- Automated DUnit test execution
- Test results in XML format
- Continue on test failures option

### Installers
- InnoSetup script compilation
- MSI generation using WiX
- Code signing of executables and installers
- MD5 checksum generation

### Deployment
- Artifact uploads to package registries
- GitHub/GitLab release creation
- Automated release notes

## Customization

To adapt this example for your project:

1. **Copy files to correct locations** (see [Installation](#installation) above)
2. Update `your-project.builder` with your project details
3. Modify paths in installer scripts to match your project structure
4. Update workflow variables and secrets with your values
5. Update the `BUILDER_FILENAME` variable in workflows to match your `.builder` file name
6. Adjust build configurations as needed
7. Configure environment protection rules in GitHub/GitLab settings

## Notes

- The workflows assume Windows runners are available
- BOBBuilder must be installed and accessible on the build agents
- Delphi compiler must be installed and configured
- Code signing requires valid certificates (optional)

## Support

For more information about BOBBuilder, see the main project documentation.
