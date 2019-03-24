# FP_PSB
Final Project of PSB

ETAPAS:

1.Faça a coleta de sinais EEG utilizando o seguinte protocolo experimental:

-Abra os olhos e mantenha sua mente focada, por 10 segundos, em uma determinada imagem internalizada, por exemplo o Sol.
-Feche os olhos e mantenha sua mente focada, por 10 segundos, em uma determinada imagem internalizada, por exemplo o Sol.
-Realize mentalmente as seguintes operações matemáticas:
X=33+55+12
X=300−33+95
X=334
X=216
Para cada uma das tarefas acima utilize um marcador para delimitar a região em que o evento ocorreu.



2.Converta os arquivos dos sinais coletados em formato Excel. Para isto utilize a toolbox disponível em https://github.com/aoandrade/PDPack.
Vocês devem utilizar as seguintes bibliotecas e programas exemplos:

-read_Intan_RHD2000_file.R
-ExampleOpenItantFile.R
-ExampleConvertIntanToExcel.R


3-Plote cada um dos canais de sinais coletados utilizando a função dygraph. Em cada um dos gráficos desenhe linhas verticais que indiquem o início e o final das atividades realizadas durante o protocolo experimental.



4-Faça a filtragem dos sinais, por meio de um filtro Butterworth, para a estimativa das ondas Delta, Teta, Alfa, Beta e Gama. Plote os resultados de forma semelhante à realizada no item 3.



5-Estime o espectro de amplitude para cada evento do item 1, e para cada uma das ondas do item 4.



6-Calcule a frequência mediana de cada espectro de amplitude do item 5.



7-Descreva como foi o comportamento da frequência mediana em cada um dos canais disponíveis e para cada um dos eventos realizados durante a coleta de dados.
