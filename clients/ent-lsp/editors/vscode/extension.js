const path = require('path');
const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

exports.activate = async () => {
  const cfg = vscode.workspace.getConfiguration('ent');
  const bbEdn = cfg.get('server.config') || path.join(__dirname, '..', '..', 'bb.edn');
  const python = cfg.get('python.enabled');
  const env = python ? process.env : { ...process.env, ENT_LSP_PYRIGHT: '0' };
  if (!python && !(await vscode.workspace.findFiles('**/*.{ent,entv,enta,entp}', null, 1)).length) {
    return;
  }
  client = new LanguageClient(
    'ent',
    'Sutra .ent',
    {
      command: cfg.get('server.command') || 'bb',
      args: ['--config', bbEdn, 'lsp'],
      options: { env },
    },
    {
      documentSelector: [
        { scheme: 'file', language: 'ent' },
        ...(python ? [{ scheme: 'file', language: 'python' }] : []),
      ],
      synchronize: {
        fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{ent,entv,enta,entp}'),
      },
    },
  );
  await client.start();
};

exports.deactivate = () => client && client.stop();
