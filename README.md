# What's My Fitted Model? (WMFM)

WMFM is an R/Shiny app for fitting statistical models and generating plain-language explanations of fitted models for teaching and learning.

## Configuring an AI provider

WMFM needs access to an AI provider before it can generate fitted-model explanations. Configure either a commercial provider or a local Ollama model before using the app.

API keys are read from environment variables. Set these variables in `~/.Renviron`, then restart R before starting WMFM. The app does not ask for API keys in the UI and does not look for them in the WMFM config file.

WMFM never stores API keys in its config file.

### Commercial providers require API credits

Claude and OpenAI access require provider API credentials with API billing or API credits enabled. A normal Claude subscription or ChatGPT Plus subscription does not automatically provide API access for an R package.

### Claude / Anthropic

Add your Anthropic API key to `~/.Renviron`:

```text
ANTHROPIC_API_KEY=your_key_here
```

Restart R after editing `~/.Renviron`. Then start WMFM, open **Settings**, choose **Claude / Anthropic**, click **Apply provider**, and click **Save provider config** if you want Claude remembered for future sessions.

### OpenAI

Add your OpenAI API key to `~/.Renviron`:

```text
OPENAI_API_KEY=your_key_here
```

Restart R after editing `~/.Renviron`. Then start WMFM and select the OpenAI provider in **Settings** when that provider is enabled by the app.

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
