# What's My Fitted Model? (WMFM)

WMFM is an R/Shiny app for fitting statistical models and generating plain-language explanations of fitted models for teaching and learning.

## Configuring an AI provider

WMFM needs access to an AI provider before it can generate fitted-model explanations. A new desktop user should be able to do this from the app without needing to remember where every hidden configuration file lives.

There are two supported setup styles:

- **Local desktop use:** configure the provider in **Settings** and use **Provider setup** for API-key guidance or local desktop credential entry.
- **Deployed Shiny use:** the installer or administrator configures providers, models, and credentials outside the app. Ordinary browser users should not enter API keys or add providers.

### Local desktop setup

Open **Settings** in WMFM and choose the provider you want to use.

For providers that need an API key, click **Provider setup**. WMFM shows the expected environment variable, such as `ANTHROPIC_API_KEY`, and explains whether a credential is currently available. On a local desktop session, the setup dialog may also allow you to save or remove a local credential in the WMFM user configuration file.

Environment variables still take priority over locally saved credentials. This means an API key set in `~/.Renviron` or the process environment overrides any locally saved WMFM credential.

WMFM never displays API key values in the Settings tab, setup dialog, status text, logs, tests, NEWS, or README examples.

### Finding and editing `.Renviron`

For users who prefer the standard R environment-variable approach, `usethis` can open the right file:

```r
usethis::edit_r_environ()
```

After editing `.Renviron`, restart R before starting WMFM again.

### Commercial providers require API credits

Claude and OpenAI access require provider API credentials with API billing or API credits enabled. A normal Claude subscription or ChatGPT Plus subscription does not automatically provide API access for an R package.

### Claude / Anthropic

For environment-variable setup, add your Anthropic API key to `.Renviron`:

```text
ANTHROPIC_API_KEY=your_key_here
```

Restart R after editing `.Renviron`. Then start WMFM, open **Settings**, choose **Claude / Anthropic**, and use **Provider setup** to check credential status.

### OpenAI

OpenAI support should use the same setup pattern when that provider is enabled by the app:

```text
OPENAI_API_KEY=your_key_here
```

Restart R after editing `.Renviron`, then use **Settings** and **Provider setup** to select the provider and check credential status.

### Local Ollama

Ollama does not require a commercial API key. To use a local model, install and start Ollama, then pull at least one model, for example:

```bash
ollama pull llama3.1
```

In WMFM, open **Settings**, choose **Ollama (local)**, and set the Ollama base URL, usually:

```text
http://localhost:11434
```

Choose the model and save the provider configuration if you want the local provider remembered for future sessions.

Many users will have only one local Ollama model installed. That is fine: select that model and save the provider config.

WMFM only attempts Ollama model discovery when Ollama is the selected provider and a local Ollama configuration is available.

### First run with no provider configured

If WMFM starts without a usable provider, it displays setup guidance. For a local desktop user, start in **Settings** and then open **Provider setup**. For a deployed app, contact the installer or administrator because ordinary users cannot add server credentials from the browser.

### Deployed Shiny apps

In a deployed app, provider setup is administrator-managed. The installer should configure credentials through server-controlled environment variables or deployment secrets, not through browser-entered user credentials.

A deployed WMFM app should not let ordinary users enter API keys, add providers, or change server credentials. The installer may expose a limited list of approved providers or models, and ordinary users may choose only among those approved options.

Use environment variables such as `ANTHROPIC_API_KEY` for hosted providers. Never commit real API keys to package files, examples, tests, README snippets, deployment manifests, or stage artefacts.

## Developer mode

WMFM includes an optional developer mode intended for people testing, diagnosing or improving the app. It exposes additional explanation diagnostics, grading information, evaluation tools and downloadable debugging reports.

Developer mode does not unlock hidden statistical methods or additional features intended for ordinary users. Most users will have no reason to enable it.

### Enabling developer mode

Developer mode has two safeguards:

1. the developer-mode controls must be explicitly exposed; and
2. the user must enter a locally configured password to unlock them.

First, create a salted hash for the password you want to use:

```r
wmfm::makeDeveloperModePasswordHash("choose-a-password")
```

The function returns a character string containing the password hash. Copy that complete string.

Next, open your `~/.Renviron` file:

```r
usethis::edit_r_environ()
```

Add the following two lines, replacing the example hash with the value returned by `makeDeveloperModePasswordHash()`:

```text
WMFM_SHOW_DEVELOPER_MODE=1
WMFM_DEVELOPER_MODE_PASSWORD_HASH="your-complete-password-hash""
```

**Note** the quotes may not be necessary, but are advised.

Restart R after saving the file.

The next time WMFM starts, a developer-mode control will be available. When you attempt to enable it, enter the original plain-text password that you supplied to `makeDeveloperModePasswordHash()`.

Only the salted hash is stored in `.Renviron`; the plain-text password is not stored by WMFM. If you forget the password, generate a new hash from a new password and replace the value of `WMFM_DEVELOPER_MODE_PASSWORD_HASH`.

Developer mode applies only to the current WMFM session and must be unlocked again after the app is restarted. Provider settings and credentials are independent of the developer-mode password.
