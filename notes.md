## Protocolos de transporte

- Local protocol
- SSH
- Git protocol
  - Sem autenticação
  - Full-duplex
    - Bidirecional
    - Simultâneo
    - O servidor pode enviar atualização ao mesmo tempo que recebe dados do cliente.
- HTTP
- Ambos protocolos utilizam o mesmo mecanismo de conectar os comandos -pack no client e no server.
  - Uma operação de fetch significa no cliente `git fetch-pack` e no server `git upload-pack`.
  - Uma operação de push significa no cliente `git send-pack` e no server `git receive-pack`.

## Git protocol

- As fases do protocolo Git:

### Reference discovery

- Permite o cliente descobrir quais dados o servidor possui.
- Funciona vendo o commit mais recente de cada ref no server e determinando qual ref precisa atualizar no client.
- Chamado também de ref advertisement
- `ls-remote`: Lista as refs do remote.
- O cliente precisa estabelecer uma conexão TCP na porta 9418 (geralmente).
- Precisa enviar uma discovery request em pkt-line format.

### Capabilities

- O servidor possui capacidade que o cliente pode exigir para a comunicação posterior.

### Packfile negotiation

- Determinar a quantidade mínima de trabalho para o cliente ou servidor ser atualizado.
- No comando `fetch`, o cliente envia as refs que possui para o servidor enviar o packfile mínimo possível.

### pkt-line format

- Uma string binária de tamanho variável
- O tamanho é codificado nos 4 primeiros bytes do pkt-line (incluindo o comprimento do tamanho).
- Um pkt-line especial é o `0000` (tamanho 0) que significa que um ponto acordado na comunicação foi alcançado.
- Exemplo:
  - "0006a\n"
    - 2 bytes do a + \n
    - 4 bytes do tamanho
    - o comprimento é 0x0006, ou seja, 6 bytes.

### git daemon

- Executa um servidor com Git Protocol, disponibilizando repositórios.
- `git daemon --reuseaddr --verbose --base-path=. --export-all`
  - `--reuseaddr`:
    - Reabre rapidamente o socket depois do servidor ser fechado.
    - Sem essa opção, pode impedir o uso imediato da mesma porta.
  - `--base-path=.`:
    - Diretório base onde armazena os repositórios.
    - Além disso, `git://example.com/repo` procura em `./repo`.
  - `--export-all`:
    - Não necessita de um arquivo especial para exportar os repositórios.
    - Efetivamente deixa todos os diretórios Git na pasta visíveis.
  - Exemplo de uso:
    - `git clone git://<repo em outra máquina>/repo`
- Read-only server.

### Inspeção de pacotes

- Ngrep é uma ferramenta que pode ser utilizada para inspecionar pacotes de rede (pequenas unidades de dados).
- Semelhante ao GNU `grep`, mas é utilizado para pacotes de rede.
- `sudo apt install ngrep -y`
- `sudo ngrep -P "*" -d lo -W byline port 9418 and dst host localhost`
  - `-P`: Fazer correspondência do conteúdo do pacote com uma regex.
  - `-d`: Interface (lo é a interface loopback nesse exemplo)
  - `-W byline`: Mostra os pacotes linha por linha.
    - Formato de saída.
  - `port 9418 and dst host localhost`
    - Filtro
    - Porta 9418
    - Destino é para o host `localhost`
  - `-q`: não imprime informações extras, apenas os pacotes
- `*` indica um caractere não printável
- Saída:
  - `####`: Começo da captura.
  - `T`: TCP.
  - `[AP]`: Flags do protocolo.
    - `A`:
      - Indica que o pacote capturado contém um acknowledgment, ou seja, o destinário confirmou o recebimento dos pacotes.
      - Acknowledgment (reconhecimento) no protocolo TCP.
        - Garante que o destinário receba o pacote.
        - Quando um participante da comunicação envia um pacote para outro, ele espera receber uma confirmação (ACK) como resposta.
        - Se não receber dentro de um certo intervalo de tempo, ele irá reenviar o pacote.
        - Quando o receptor recebe o pacote, ele envia um pacote confirmando o recebimento de um pacote em específico pelo seu _número de sequência_.
          - Outras flags:
            - SYN: Iniciar a conexão.
            - FIN: Finalizar a conexão.
            - ACK: Confirmação de recebimento de um pacote de dados.
    - `P`: Push.
      - Instrui o receptor a passar os dados para a camada de aplicação imediatamente, sem colocar em um buffer.
  - `##`: indica a transição entre pacotes ou grupos de pacotes.

### Modelagem de tráfego / Traffic shaping

- priorizar o tráfego de aplicações
- testar como o aplicativo se sairia em diferentes cenários.
- ipfw (IP firewall):
  - firewall e controla o tráfego
  - bsds apenas
  - pode ser utilizado com dummynet (permite modelagem de tráfego)
  - pipes
    - definir a quantidade de bandwidth que pode ser utilizado por determinado tráfego
    - `ipfw pipe 1 config bw 20KBytes/s`
      - Cria um _tubo_ com id 1
      - Configura a largura de banda
      - 20 kilobytes por segundo
    - `ipfw add 1 pipe 1 src-port 9418`
      - `ipfw add 1`: adiciona uma nova regra com prioridade 1 (maiores números são alta prioridade)
      - `pipe 1`: regra associada ao pipe 1
      - `src-port 9418`: deve aplicar a pacotes com porta de origem sendo 9418
    - `ipfw delete 1`:
      - remove a regra com número 1
      - cada regra tem um número que permite ordenar e identificar cada uma

## ABNF

- Augmented Backus-Naur Form.
- Extensão da Baclus-Naur Form (BNF).
- Notação para descrever sintaxe de linguagens formais, incluindo protocolos de comunicação como é o caso.
- Cada regra é composta por um identificador e uma definição que pode incluir sequências de elementos, escolhas (/) e repetições.
- Componentes:
  - Literais: Elementos fixos.
  - Espaço: Separar elementos (SP).
  - NUL: Byte nulo.
  - Intervalos de valores: %x01-ff representa todos os bytes, com exceção do NUL.
- Notação:
  - \*: zero ou mais

## Flags úteis

- `GIT_TRACE_PACKET=1`: Ativa log dos packets entre server e client.
- `GIT_TRACE=1`: Informações gerais de debug da execução do comando.
- `GIT_CURL_VERBOSE=1`: Apenas no transporte HTTP. Mostra informação de debug do cURL em modo detalhado.
- `GIT_DEBUG_SEND_PACK=1`: Habilita debug no comando `upload-pack`.
- `GIT_TRANSPORT_HELPER_DEBUG=1`: Habilita debug em remote helpers.

## TODOs / Deep Dives

- CS
  - Networking
    - Packets
    - Interfaces
  - PLT
    - Formal languages
    - ABNF, BNF
