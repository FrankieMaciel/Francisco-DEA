import os
import glob

def apagar_txt_recursivamente(pasta):
    # Percorre todos os arquivos .txt na pasta e subpastas
    for arquivo in glob.glob(os.path.join(pasta, '**', '*.txt'), recursive=True):
        try:
            # Remove o arquivo
            os.remove(arquivo)
            print(f'Arquivo removido: {arquivo}')
        except Exception as e:
            print(f'Erro ao remover {arquivo}: {e}')

# Exemplo de uso
caminho_da_pasta = './Dataset-de-teste'  # Altere para o caminho da sua pasta
apagar_txt_recursivamente(caminho_da_pasta)
