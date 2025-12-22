## Thick Client (Windows tray shell) — Technical Design & Delivery Playbook

### 1) Purpose and scope
This document specifies the technology stack, development standards, SDLC, and CI/CD approach for a **Windows desktop “thick client”** that:

- Runs as a **tray-resident application** (notification area icon).
- Provides a **tray menu** for common actions (Open UI, Switch environment, View logs, About, Exit).
- Hosts an existing **web-based UI** inside an embedded browser control.
- Applies enterprise-grade controls: **code signing**, **MSIX packaging**, **centralized deployment**, **auditable updates**, and **secure WebView2 hardening**.

**Non-goals** (explicit):
- Rewriting the web UI as a native UI.
- Building a general-purpose browser.
- Bypassing corporate security controls (proxy/TLS inspection/EDR/DLP).

---

### 2) Technology choices (Windows-first / bank-grade)
#### 2.1 Primary rationale
For a Windows-dominant bank, the optimal balance of **longevity, vendor trust, endpoint management fit, and security posture** is:

- **.NET (LTS) + WinUI 3 (Windows App SDK)** for the shell
- **Microsoft Edge WebView2 (Evergreen Runtime)** for rendering the web UI
- **MSIX** for packaging + deployment + clean updates

This keeps the runtime under Microsoft’s support and patching model, aligns to Windows enterprise management, and avoids the “we ship our own Chromium” burden.

#### 2.2 Version-pinned baseline (as of 2025-12-18)
These versions are the **reference baseline** for the initial implementation and CI images. Updates are governed by the upgrade process in section 10.

| Category | Technology | Version | Notes |
|---|---:|---:|---|
| Language/runtime | .NET SDK | **10.0.101** | LTS baseline; pinned via `global.json` |
| IDE (dev + CI agents) | Visual Studio 2022 | **17.14.22** | Recommended for WinUI/MSIX tooling |
| Windows SDK | Windows 11 SDK | **10.0.26100.7175** | For building/packaging |
| UI framework | Windows App SDK (WinUI 3) | **1.8.251106002** | `Microsoft.WindowsAppSDK` |
| Embedded browser SDK | WebView2 SDK | **1.0.3650.58** | `Microsoft.Web.WebView2` |
| Embedded browser runtime | WebView2 Runtime (Evergreen, x64) | **143.0.3650.80** | Enterprise-managed distribution |
| Tray icon | H.NotifyIcon.WinUI | **2.3.1** | Tray icon + context menu |
| MVVM utilities | CommunityToolkit.Mvvm | **8.4.0** | Commands, observable objects |
| Resilience | Polly | **8.6.5** | Retries/timeouts (shell networking) |
| Logging | Serilog | **4.3.0** | Structured logging (optional) |
| Logging sink | Serilog.Sinks.File | **7.0.0** | File logs for support |
| Windows Event Log | Microsoft.Extensions.Logging.EventLog | **10.0.1** | Optional: Event Log provider for SOC/service desk |
| Test framework | xUnit | **2.9.3** | Unit/integration tests |
| Assertions | FluentAssertions | **8.8.0** | Readable assertions |
| Coverage | coverlet.collector | **6.0.4** | Coverage collection |
| E2E (web) | Microsoft.Playwright | **1.57.0** | Browser automation for web UI |
| Reports | ReportGenerator | **5.5.1** | Coverage report rendering |
| Versioning | Nerdbank.GitVersioning | **3.9.50** | Deterministic SemVer + build metadata |
| SBOM | CycloneDX (`dotnet tool`) | **2.5.0** | Generate CycloneDX SBOMs in CI |

**Note on WebView2 runtime**: use **Evergreen** mode for security servicing. Where endpoint policy forbids auto-update, use Evergreen but manage updates via **Intune/Configuration Manager/WSUS**.

#### 2.3 Visual Studio workloads (developer machines + CI runners)
Install Visual Studio 2022 / Build Tools with (minimum):
- **.NET desktop development**
- **Windows App SDK / WinUI** tooling (templates)
- **MSIX Packaging Tools**
- **Windows 11 SDK** (10.0.26100.7175)

---

### 3) High-level architecture
#### 3.1 Process model
- **Single process** WinUI 3 app running in the background.
- Starts on user logon (optional, controlled by policy + installer).
- Owns tray icon/menu and a main window containing WebView2.

#### 3.2 Components
- **Tray Host**: creates tray icon, context menu, and handles commands.
- **Shell Window**: WinUI 3 window hosting the WebView2 control.
- **Navigation & Policy**: allowlist/denylist of URLs, window.open handling, download blocking rules, external link routing.
- **Configuration**: environment selection (DEV/UAT/PROD), feature flags, proxy settings (usually OS), logging level.
- **Telemetry & Logging**: local log files + optional Windows Event Log forwarding.

#### 3.3 IPC / host integration
Keep the boundary narrow:
- Prefer `window.chrome.webview.postMessage()` (web → host) and `CoreWebView2.PostWebMessageAsJson()` (host → web).
- Only implement **explicit commands** (e.g., “open settings”, “copy diagnostic bundle”).
- Never expose generic native APIs to the web app.

---

### 4) Security requirements and hardening (mandatory)
#### 4.1 Trust model
- The embedded UI is treated as **high-trust** only when loaded from **approved origins** (bank domains).
- All other content is treated as untrusted and must be blocked or opened externally.

#### 4.2 WebView2 hardening checklist
- **Origin allowlist**: block navigation to non-approved origins.
- **Disable devtools** in production builds (or gate behind admin policy).
- **Disable remote debugging**.
- **Window creation policy**: intercept new windows; open external URLs in system browser; keep the embedded view single-origin.
- **Download policy**: block by default; allow only if business-approved and scanned.
- **User data folder**: set explicit `userDataFolder` under `%LOCALAPPDATA%\\{Company}\\{App}\\WebView2` for predictable cleanup.
- **Certificate & proxy**: rely on Windows trust store and corporate proxy; do not implement custom TLS bypass.
- **Least privilege**: per-user install by default; no admin elevation at runtime.

#### 4.3 Supply chain controls
- SBOM generation for MSIX release artifacts.
- Dependency scanning (NuGet) + license allowlist.
- Signed builds only; unsigned builds blocked from production distribution.

#### 4.4 Authentication / SSO (typical bank scenario)
When the embedded app uses corporate SSO:
- Prefer **standard browser-based auth** within WebView2 (OIDC/SAML front-channel flows) so identity teams can enforce policy (MFA, device compliance).
- Use WebView2’s supported SSO integration where applicable (e.g., OS primary account) and keep it **policy-controlled**.
- Treat authentication state as web state (cookies/tokens) stored in the WebView2 user data folder; do not copy tokens into the host unless explicitly required and approved.

---

### 5) Project layout (recommended)
A minimal structure that stays maintainable:

- `src/ThickClient.Shell/` — WinUI 3 app + tray + WebView2 host
- `src/ThickClient.Shell.Core/` — policies, config, domain logic (testable)
- `tests/ThickClient.Shell.Core.Tests/` — unit tests
- `tests/ThickClient.Shell.IntegrationTests/` — integration (policy/config/logging)
- `eng/` — build scripts (bootstrap, signing, packaging helpers)
- `docs/` — operational docs + release notes

---

### 6) Development approach
#### 6.1 Branching
Recommended: **trunk-based development**.
- `main` is always releasable.
- Short-lived feature branches.
- Release tags (e.g., `thickclient/v1.2.3`).

#### 6.2 Coding standards
- Nullable enabled (`<Nullable>enable</Nullable>`)
- Treat warnings as errors in CI (`<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`)
- Roslyn analyzers enabled (Microsoft + security analyzers)
- Central package management (`Directory.Packages.props`) to keep upgrades consistent.

#### 6.3 Configuration
- Environment selection (DEV/UAT/PROD) via:
  - installer defaults + policy override, and
  - a tray menu selector (if allowed).
- Never embed secrets in the thick client; use the web app’s auth flow.

#### 6.4 One-command developer setup (recommended)
Provide `eng/bootstrap.ps1` (committed) that:
- Installs prerequisites via `winget` where permitted (VS Build Tools, .NET SDK, Git).
- Verifies Windows SDK + MSIX tooling presence.
- Restores NuGet packages and `dotnet tool restore`.
- Prints a “ready” summary (SDKs detected, WebView2 runtime detected, environment URLs configured).

---

### 7) Testing strategy
#### 7.1 Test pyramid (pragmatic for WebView-host shells)
- **Unit tests (many)**: URL policy, command routing, config parsing, logging, diagnostics bundle composition.
- **Integration tests (some)**: instantiate shell services (DI), validate allowlist behavior, validate telemetry/logging output.
- **E2E tests (few, high value)**:
  - Primary coverage belongs to the **web UI** (Playwright against the hosted web app).
  - Desktop shell E2E is limited to **smoke tests** (launch app, tray menu works, loads PROD URL, blocks non-allowlisted URL, exit works).

#### 7.2 Desktop UI automation (optional / nightly)
If required by audit or risk:
- Use **Windows Application Driver** (Appium Windows Driver / WinAppDriver replacement) on a dedicated Windows runner.
- Run nightly or per-release due to higher flakiness and environment sensitivity.

#### 7.3 Coverage and quality gates
- Minimum coverage gate for `*.Core` projects.
- Static analysis and security scanning gates for `main` merges.

---

### 8) Build, packaging, and release (MSIX)
#### 8.1 Packaging
- Package as **MSIX**.
- Sign MSIX with corporate code signing certificate.
- Support:
  - **per-user install** (default), and
  - per-machine (only if necessary and approved).

#### 8.2 Update model
Preferred (enterprise-friendly):
- **App Installer / MSIX automatic updates** from an internal HTTPS distribution endpoint.
Alternative:
- Intune/Configuration Manager managed deployments per ring (pilot → broad).

#### 8.3 Release rings
- **Ring 0**: Dev team
- **Ring 1**: Pilot users (IT + selected business)
- **Ring 2**: Broad deployment

#### 8.4 Signing & key management (enterprise expectation)
- Prefer signing keys stored in **HSM / enterprise PKI** or a runner-attached certificate store with audited access.
- If a PFX must be used, store it only in an approved secrets manager, scoped to protected branches/tags, with strict rotation and approval workflows.
- Ensure the MSIX publisher identity is stable across versions to avoid side-by-side installs and update failures.

#### 8.5 MSIX update feed (App Installer model)
If using App Installer updates:
- Host MSIX + `.appinstaller` on an internal HTTPS endpoint.
- Enable ring-based endpoints (e.g., `/ring1/`, `/ring2/`).
- Pin update intervals per ring and require code-signed packages.
- Document how endpoints map to AD groups / Intune assignments.

---

### 9) GitLab CI/CD (Windows runners)
#### 9.1 Non-negotiable requirement
WinUI 3/MSIX builds require a **Windows GitLab Runner** (Shell executor or Docker Windows containers, depending on your setup). Linux runners will not build MSIX WinUI 3 reliably.

#### 9.2 Build agent baseline
On the Windows runner image/VM:
- Visual Studio 2022 **17.14.22** (or Build Tools + required workloads)
- Windows 11 SDK **10.0.26100.7175**
- .NET SDK **10.0.101**
- Signing toolchain (certificate access via secure store)

#### 9.3 Pipeline stages (recommended)
- **validate**: format/analyzers, dependency/license checks
- **build**: restore + compile
- **test**: unit + integration + coverage
- **package**: MSIX packaging
- **sign**: sign MSIX
- **publish**: artifacts + release notes
- **deploy (optional)**: publish to ring distribution

#### 9.4 Example `.gitlab-ci.yml` skeleton (Windows runner)
```yaml
stages:
  - validate
  - build
  - test
  - package
  - sign
  - publish

variables:
  DOTNET_CLI_TELEMETRY_OPTOUT: "1"
  DOTNET_NOLOGO: "true"

default:
  tags:
    - windows

validate:
  stage: validate
  script:
    - dotnet --info
    - dotnet restore
    - dotnet build -c Release -warnaserror

build:
  stage: build
  script:
    - dotnet restore
    - dotnet build -c Release --no-restore
  artifacts:
    expire_in: 7 days
    paths:
      - src/**/bin/Release/

test:
  stage: test
  script:
    - dotnet test -c Release --no-build --collect:"XPlat Code Coverage" --results-directory TestResults
  artifacts:
    when: always
    expire_in: 14 days
    paths:
      - TestResults/

package:
  stage: package
  script:
    # Typically: msbuild packaging project /p:Configuration=Release
    # or dotnet publish with MSIX tooling depending on project template.
    - msbuild src/ThickClient.Shell/ThickClient.Shell.csproj /t:Publish /p:Configuration=Release
  artifacts:
    expire_in: 30 days
    paths:
      - src/ThickClient.Shell/AppPackages/

sign:
  stage: sign
  rules:
    - if: '$CI_COMMIT_BRANCH == "main"'
  script:
    # SignTool usage depends on cert storage model (PFX in secret, or HSM/KeyVault-backed).
    - powershell -File eng/sign-msix.ps1 -InputPath "src/ThickClient.Shell/AppPackages" 
  artifacts:
    expire_in: 180 days
    paths:
      - src/ThickClient.Shell/AppPackages/

publish:
  stage: publish
  rules:
    - if: '$CI_COMMIT_BRANCH == "main"'
  script:
    - powershell -File eng/publish-artifacts.ps1
```

**Notes**:
- Keep signing material out of GitLab variables unless your bank permits it; prefer a **runner-attached certificate store/HSM/KeyVault** integration.
- Use caching carefully on Windows to avoid stale MSIX outputs.

#### 9.5 Security and compliance jobs (recommended)
Add GitLab security templates where possible (often run on Linux runners, even if the build is Windows):
- SAST
- Dependency scanning (NuGet)
- Secret detection
- License compliance

Additionally, add an SBOM job:
- `dotnet tool restore`
- `dotnet CycloneDX ...` (generate CycloneDX JSON/XML)
- Publish SBOM as an artifact attached to the release.
#### 9.6 Runner provisioning approach
To keep builds reproducible:
- Maintain a **golden Windows runner image** (Packer/VM template) with the pinned VS + SDK versions.
- Update the runner image on a controlled cadence (monthly/quarterly) and validate with a pipeline “runner certification” job.
- Keep at least two runners per environment for availability.

---

### 10) Upgrade and longevity strategy (how we stay current safely)
#### 10.1 Pinning model
- Pin the SDK in `global.json` (example: `10.0.101`).
- Pin NuGet dependencies centrally (`Directory.Packages.props`).
- Track the WebView2 Runtime as a **deployment dependency** (not shipped inside the app).

#### 10.2 Upgrade cadence (bank-friendly)
- **Monthly**: patch upgrades (.NET servicing, WebView2 runtime servicing), security fixes.
- **Quarterly**: Windows App SDK upgrades (validate UI/regressions).
- **Annually or per-LTS**: .NET LTS upgrade planning.

#### 10.3 Automated dependency updates
- Use Renovate (preferred) or GitLab Dependency Update tooling to open merge requests for:
  - NuGet updates (grouped by risk: patch/minor/major)
  - CVE-driven updates (priority)
- Pipeline enforces: build + tests + basic smoke packaging on each dependency MR.

#### 10.4 Compatibility matrix
Maintain a small matrix (in the repo) for each release:
- App version
- Minimum Windows build
- Windows App SDK version
- WebView2 SDK version
- Minimum WebView2 Runtime version

#### 10.5 Rollback plan
- MSIX supports clean uninstall/reinstall.
- Keep previous MSIX versions available in the distribution endpoint.
- Ringed rollout ensures controlled blast radius.

#### 10.6 “Easy upgrades” operating procedure (repeatable)
For each dependency update (monthly batch or CVE hotfix):
- Renovate/GitLab opens an MR with version bumps.
- CI runs: build, unit/integration tests, packaging smoke.
- Optional: publish a Ring 0 canary artifact.
- Release manager approves after Ring 0/1 validation.
- Tag + sign + publish to Ring 1, then Ring 2.
- Record the updated compatibility matrix entry.

---

### 11) Operational support
- Local log files under `%LOCALAPPDATA%\\{Company}\\{App}\\Logs`.
- “Collect diagnostics” menu action:
  - zips logs + basic system info (OS build, runtime versions) for service desk.
- Respect PII controls: no sensitive payload logging.

---

### 12) Implementation notes for the tray + WebView shell
- Tray icon app is the **primary entry point**; main window can be hidden instead of closed.
- Provide menu actions:
  - Open application
  - Reload
  - Switch environment (optional)
  - View logs
  - About (versions: app + WindowsAppSDK + WebView2 SDK + detected runtime)
  - Exit
- Enforce URL policy at the host level; never rely on the web app alone.

---

### 13) Reference links (vendor-trusted)
- Windows App SDK: `https://github.com/microsoft/WindowsAppSDK`
- WebView2 docs: `https://learn.microsoft.com/microsoft-edge/webview2/`
- WebView2 release notes: `https://learn.microsoft.com/microsoft-edge/webview2/release-notes/`
- MSIX packaging: `https://learn.microsoft.com/windows/msix/`
- Visual Studio 2022 release notes: `https://learn.microsoft.com/visualstudio/releases/2022/release-notes`

---

### 14) Context7 validation notes (usage correctness) + “latest version” verification procedure
You asked to validate both **(a) usage correctness** and **(b) “latest stable” dependency versions** using Context7.

#### 14.1 What Context7 validated here
Context7 is strongest for **authoritative usage patterns** (official docs + upstream READMEs). We used it to validate the recommended approach and usage patterns for:

- **Windows App SDK / MSIX versioning constraints**: Windows App SDK versioning and how it maps to MSIX package versioning and release channels (stable vs non-stable tags). Source: `WindowsAppSDK/specs/Deployment/MSIXPackageVersioning.md` (Context7 library: `/microsoft/windowsappsdk`).
- **Tray icon control for WinUI**: WinUI usage of `H.NotifyIcon`’s `TaskbarIcon`, menu activation, and recommended XAML patterns. Source: `H.NotifyIcon` README (Context7 library: `/havendv/h.notifyicon`).

Concrete tray icon example (WinUI) from upstream docs (use as reference when implementing):

```xml
<Window
    xmlns:tb="using:H.NotifyIcon"
    >
    <tb:TaskbarIcon
        ToolTipText="ToolTip"
        IconSource="/Images/TrayIcons/Logo.ico"
        ContextMenu="{StaticResource TrayMenu}"
        MenuActivation="LeftOrRightClick"
        />
</Window>
```

#### 14.2 “Latest version” verification: why Context7 alone is not sufficient
Context7 documentation is not a live package registry and will not reliably answer: “what is the latest stable version on NuGet today?” for every dependency.

For **exact latest stable versions**, the bank-grade sources of truth are:
- NuGet gallery metadata (for packages)
- Microsoft release notes (for WebView2 Runtime / Visual Studio / Windows SDK / .NET servicing)

That’s why this document pins versions **as-of 2025-12-18** and also defines a repeatable verification process below.

#### 14.3 Repeatable verification commands (dev + CI)
Use these commands to re-validate “latest” at any time:

- **Check outdated NuGet packages (per solution)**:

```powershell
dotnet list package --outdated
```

- **Check SDK version pinned vs installed**:

```powershell
dotnet --info
```

- **Validate WebView2 Runtime presence and version on endpoints** (example approach; exact method may vary by enterprise tooling):
  - Prefer inventory via Intune/ConfigMgr (recommended).
  - Locally, confirm runtime presence in “Apps & features” (Evergreen runtime).

#### 14.4 “Latest” governance (what we enforce)
- **CI is pinned** (reproducible builds), upgrades land via controlled MRs.
- **WebView2 Runtime is serviced** (Evergreen via enterprise update tooling).
- **Monthly servicing rhythm** with fast CVE hotfix path (see section 10.6).

---

### 15) CTO review (industry-aligned) + decisions to implement (automation-first)
This section converts the approach into **explicit decisions** that engineering must implement to meet typical bank expectations: **secure SDLC**, **auditability**, **repeatability**, and **automated controls**.

#### 15.1 CTO review of the approach (what is strong / what needs tightening)
- **Strong**:
  - **WinUI 3 + WebView2 Evergreen** is the correct vendor-aligned choice for a Windows-dominant bank. It aligns with endpoint patching and avoids bundling a browser (unlike Electron).
  - **MSIX** is the right packaging format for clean install/uninstall, enterprise deployment, and controlled updates.
  - Pinning versions **as-of-date** + ringed rollout is consistent with regulated change control.
- **Needs tightening for industry expectations**:
  - Define **security gates** as executable pipeline jobs (not just policy text).
  - Define **release evidence** (artifacts, SBOM, signing proof, test reports) as mandatory outputs.
  - Define **hard runtime policies** for WebView2 (navigation allowlist, download policy, devtools/debugging, external link handling) as non-optional requirements with tests.
  - Define **endpoint deployment responsibility split**: app package vs WebView2 Runtime servicing.

#### 15.2 Decisions to implement (MUST)
These are “musts” for a bank-grade thick client. Treat them as requirements, not suggestions.

- **Architecture / runtime**:
  - **MUST** run as a **tray-resident application** with a single-instance guard (second launch activates existing instance).
  - **MUST** host web UI in **WebView2** and enforce an **origin allowlist** at the host layer (block/redirect everything else).
  - **MUST** block or strictly control:
    - New windows (`window.open`) → open in system browser or block
    - Downloads → cancel by default (allow only by explicit requirement + scanning story)
    - DevTools and remote debugging in production
  - **MUST** use a dedicated `userDataFolder` and include a tray action to **clear cache/site data** (support + privacy).

- **Secure SDLC**:
  - **MUST** use protected `main` with:
    - required pipelines
    - required code review approvals
    - required security approval for “high risk” changes (auth, WebView policies, update/signing)
  - **MUST** run automated checks on every merge request:
    - compile
    - unit tests
    - coverage (for `*.Core`)
    - SBOM generation
    - dependency scanning + license compliance
    - secret detection
  - **MUST** produce immutable build artifacts with retention and traceability to commit SHA.

- **Packaging & deployment**:
  - **MUST** ship as **MSIX** with a stable publisher identity and a stable package family name.
  - **MUST** sign MSIX in CI using enterprise key management (HSM / enterprise PKI / controlled certificate store).
  - **MUST** separate concerns:
    - Thick client MSIX is updated by App Installer feed or Intune rings
    - WebView2 Runtime Evergreen is serviced by enterprise endpoint tooling

- **Observability & supportability**:
  - **MUST** implement structured logging with PII-safe defaults (no tokens, no payloads).
  - **MUST** provide a “Collect diagnostics” action that generates a support bundle (logs + versions + policy snapshot).
  - **MUST** stamp the About dialog with versions: app, commit SHA, Windows App SDK, WebView2 SDK, detected WebView2 Runtime.

#### 15.3 Decisions to implement (SHOULD)
- **SHOULD** run a minimal nightly desktop smoke automation on a dedicated Windows runner/VM.
- **SHOULD** include an ADR process (section 16.2) for all material changes.
- **SHOULD** include release notes generation and a CHANGELOG discipline.

---

### 16) Secure SDLC & governance model (bank-grade)
This section maps engineering practice to typical bank controls (independent of any specific framework like ISO/NIST).

#### 16.1 Standards alignment (practical)
Implement controls that map well to:
- **Microsoft SDL** principles (threat modeling, secure coding, security reviews).
- **NIST SSDF** outcomes (secure development environment, secure build pipeline, artifact integrity).
- **OWASP SAMM** maturity themes (governance, design, implementation, verification, operations).

#### 16.2 Architecture Decision Records (ADR) – mandatory for key changes
Create `docs/adr/` and require an ADR for changes that impact:
- WebView2 security policy (allowlists, host-object bridging, downloads, debugging/devtools)
- update model (MSIX feed, Intune rings, startup behavior)
- authentication/SSO integration decisions
- signing and key custody

ADR template (minimum fields):
- Context, decision, alternatives considered, security implications, rollout plan, rollback plan, evidence required.

#### 16.3 Change management and release governance
- All releases are created from **protected tags** (e.g., `thickclient/vX.Y.Z`).
- Releases require:
  - successful pipeline on the tag
  - signed MSIX
  - SBOM attached
  - test reports attached (unit/integration, and smoke if enabled)
  - security scan results attached (or referenced by pipeline ID)

---

### 17) Automation blueprint (repo assets to implement)
This document recommends adding the following automation assets to the repository to minimize manual work and reduce “it works on my machine” risk:

#### 17.1 Deterministic builds and centralized dependency management
- `global.json` pins **.NET SDK**.
- `Directory.Packages.props` pins all **NuGet** packages centrally.
- `Directory.Build.props` enforces:
  - `TreatWarningsAsErrors`
  - nullable
  - analyzers enabled
  - deterministic build where possible
- `.config/dotnet-tools.json` pins tool versions (SBOM, report generation, etc.).

#### 17.2 Dependency automation
- Use Renovate (preferred) to open MRs for:
  - patch/minor updates (auto-merge optional after green pipeline, per policy)
  - major updates (manual review + ADR if impactful)
  - CVE-related updates (priority)

#### 17.3 “One command” scripts
Commit PowerShell scripts under `eng/`:
- `eng/bootstrap.ps1` – install/verify prerequisites + restore tools/packages
- `eng/build.ps1` – restore + build
- `eng/test.ps1` – run tests + coverage
- `eng/package.ps1` – build MSIX
- `eng/sign.ps1` – sign MSIX from secure key source
- `eng/sbom.ps1` – generate SBOM
- `eng/publish.ps1` – publish artifacts and (optionally) update ring feed

Each script prints machine-readable summaries (JSON) to support CI parsing and to keep release evidence consistent.

---

### 18) GitLab CI/CD – hardened, automation-heavy blueprint
The current section 9 is a good start; below is what we enforce in regulated environments.

#### 18.1 Split workloads across runners (recommended)
- **Windows runners**: build, package, sign (WinUI/MSIX).
- **Linux runners**: GitLab security templates (SAST, dependency scanning), secret detection, license checks (where supported).

#### 18.2 Pipeline “must have” artifacts (evidence)
Every pipeline on `main` and every release tag must publish:
- compiled binaries
- test results (JUnit/TRX) + coverage reports
- SBOM (CycloneDX)
- signed MSIX package(s)
- a manifest JSON describing: version, commit SHA, build timestamp, toolchain versions

#### 18.3 Enforced quality gates (automated)
Minimum gates to wire into GitLab as required jobs:
- **Format/analyzers**: `dotnet format` (verify mode) + analyzers as errors
- **Unit/integration tests**: must pass
- **Coverage gate**: enforce threshold for `*.Core` (example: 70% line; calibrate to your org)
- **Dependency + license compliance**: must pass
- **SBOM generation**: must succeed and be attached

#### 18.4 Signing automation design (enterprise-safe)
Signing must be non-interactive and auditable:
- Preferred: signing key is in enterprise **HSM/PKI**, runner can request signing operation with audited identity.
- Acceptable: runner has access to a certificate in Windows certificate store with strict ACLs + audited usage.
- Avoid: storing PFX in GitLab variables unless explicitly approved by security and compensated with strict controls.

---

### 19) WebView2 security policy (make it concrete and testable)
Convert section 4 from a checklist into explicit policy that can be unit-tested.

#### 19.1 Navigation allowlist (required)
Define:
- Allowed origins (exact hostnames; wildcard only if explicitly approved)
- Allowed URL schemes (`https:`; optionally `http:` only for DEV)
- Explicit denylist for `file:`, `data:`, `javascript:`, `ws:` unless required

Unit tests must validate:
- allowed URL passes
- non-allowlisted URL is blocked and logged
- `window.open` is handled per policy

#### 19.2 Download policy (default deny)
Enforce cancellation of downloads unless a business-approved allowlist exists.
If downloads are enabled:
- require file type allowlist
- require destination restrictions (no arbitrary paths)
- define malware scanning and DLP handling (usually via enterprise tooling)

#### 19.3 IPC message contract (explicit)
Define a JSON schema for messages:
- message name
- version
- payload constraints
- authentication/authorization expectations (if any)

Unit tests must validate:
- unknown messages are rejected
- malformed JSON is rejected
- schema version mismatch is rejected or handled explicitly

---

### 20) Endpoint deployment automation (rings)
Treat deployment as code and automate ring promotion:

- **Ring 0**: automatic publish on `main` (internal only) – optional
- **Ring 1**: manual approval job in GitLab → publish to pilot endpoint/Intune assignment
- **Ring 2**: manual approval job in GitLab → publish to broad endpoint/Intune assignment

Promotion requires evidence:
- ring smoke checks completed
- no open high-severity issues
- security scans are green or waived with recorded risk acceptance
