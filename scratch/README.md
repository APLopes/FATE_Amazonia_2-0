# GEE Assets — Validação de acesso

Scripts para testar a acessibilidade de todos os assets do Google Earth Engine
referenciados nos códigos FATE-SEEG.

## Uso

1. Abra o [Google Earth Engine Code Editor](https://code.earthengine.google.com/)
2. Cole o conteúdo de `test_gee_assets.js`
3. Clique em **Run**
4. Expanda a aba **Console** e clique em **Run** novamente se necessário
5. Copie o output (formato TSV) para conferência

## Arquivos

| Arquivo | Descrição |
|---------|-----------|
| `test_gee_assets.js` | Script principal: testa todos os 26 assets |
| `resultado_assets.tsv` | Template com a lista completa. Preencher coluna `TESTADO` após rodar o script |

## Classificação dos resultados

| `TESTADO` | Significado |
|-----------|-------------|
| `OK` | Asset acessível |
| `NOT_FOUND` | Asset foi removido ou renomeado |
| `ACCESS_DENIED` | Asset existe mas sem permissão de leitura |
| `TIMEOUT` | Timeout na requisição (possível asset muito grande) |
| `ERRO` | Outro erro |

## Legenda dos grupos

- **MAPBIOMAS** — `projects/mapbiomas-workspace/...`
- **INPE** — `projects/ee-seegfiredyn/...` e `projects/ee-seeg-brazil/...`
- **USUARIO** — `users/camilaflorestal/...`
- **PUBLICO** — `MODIS/...`
- **MODULO** — `users/gena/packages:style`
