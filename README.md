# What's My Fitted Model? (WMFM)

WMFM is an R/Shiny app for fitting statistical models and generating plain-language explanations of fitted models for teaching and learning.

## Configuring an AI provider

WMFM needs access to an AI provider before it can generate fitted-model explanations. Configure either a commercial provider or a local Ollama model before using the app.

API keys are read from environment variables. Set these variables in `~/.Renviron`, then restart R before starting WMFM. The app does not ask for API keys in the UI and does not look for them in the WMFM config file.

WMFM never stores API keys in its config file. Never paste an API key into an assignment, email, screenshot, public repository, or example script. Treat it like a password.

### Commercial providers require API credits

Claude and OpenAI access require provider API credentials with API billing or API credits enabled. A normal Claude subscription or ChatGPT Plus subscription does not automatically provide API access for an R package.

### Getting an Anthropic API key

To use Claude through Anthropic, you need an Anthropic Console account and an API key. The official Anthropic API overview is at <https://platform.claude.com/docs/en/api/overview>.

A typical setup is:

1. Go to <https://console.anthropic.com/> and sign in or create an account.
2. Check that API billing or credits are available for the account.
3. Open the API keys area in the Console.
4. Create a new key and copy it once. Store it somewhere private, such as a password manager.
5. Add the key to `~/.Renviron` as shown below.

```text
ANTHROPIC_API_KEY=your_key_here
```

Restart R after editing `~/.Renviron`. Then start WMFM, open **Settings**, choose **Claude / Anthropic**, click **Apply provider**, and click **Save provider config** if you want Claude remembered for future sessions.

### Getting an OpenAI API key

To use OpenAI, you need an OpenAI platform account and an API key. The official OpenAI API quickstart is at <https://developers.openai.com/api/docs/quickstart>.

A typical setup is:

1. Go to <https://platform.openai.com/> and sign in or create an account.
2. Check that API billing or credits are available for the account.
3. Open the API keys area in the platform dashboard.
4. Create a new key and copy it once. Store it somewhere private, such as a password manager.
5. Add the key to `~/.Renviron` as shown below.

```text
OPENAI_API_KEY=your_key_here
```

Restart R after editing `~/.Renviron`. Then start WMFM and select the OpenAI provider in **Settings** when that provider is enabled by the app.

### Editing `~/.Renviron`

In RStudio, one convenient way to open the file is:

```r
usethis::edit_r_environ()
```

If you do not use RStudio or do not have `usethis` installed, create or edit a plain text file named `.Renviron` in your home directory. Put each key on its own line, save the file, and restart R before starting WMFM.

Do not put quotation marks around the key unless the provider explicitly includes them as part of the key. Do not add spaces around the equals sign.

### Local Ollama

To use a local model, install and start Ollama, then pull at least one model, for example:

```bash
ollama pull llama3.1
```

In WMFM, open **Settings**, choose **Ollama (local)**, set the Ollama base URL, usually:

```text
http://localhost:11434
```

Choose the model, click **Apply provider**, and click **Save provider config** if you want the local provider remembered for future sessions.

Many users will have only one local Ollama model installed. That is fine: select that model and save the provider config.

WMFM only attempts Ollama model discovery when Ollama is the selected provider and a local Ollama configuration is available.

### First run with no provider configured

If WMFM starts without a configured commercial-provider API key and without saved Ollama settings, it displays a setup message and directs you back to this README section. Configure a provider through `~/.Renviron` or through the saved local Ollama settings before using AI-generated explanations.
