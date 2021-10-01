-- Aluno: Breno de Jesus Silva
-- Matrícula:	202100060702

type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type IdadeCidadao = Int
type Data = (Dia, Mes, Ano)
type DataAtual = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String 
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)
type CadastroSUS = [Cidadao]

dataBank :: CadastroSUS
dataBank =
 [
 (26716347665, "Paulo Souza"   , 'M', (11,10,1996), "Rua A, 202", "Muribeca"  , "SE", "999997000", "psouza@gmail.com"),
 (87717347115, "Ana Reis"      , 'F', (5,4,1970)  , "Rua B, 304", "Aracaju"   , "SE", "999826004", "areis@gmail.com" ),
 (42134242134, "Breno Silva"   , 'M', (16,06,2002), "Rua P, 470", "Olindina"  , "BA", "759939129", "breno@gmail.com" ),
 (64161364166, "Leila Santos"  , 'M', (23,5,1993) , "Rua Z, 730", "Serrinha"  , "BA", "999831474", "leila@gmail.com" ),
 (85685484588, "Beatriz Lima"  , 'F', (1,7,1999)  , "Rua F, 918", "Itapicuru" , "BA", "999481261", "bial@gmail.com"  ),
 (13786358324, "Murilo Cardoso", 'M', (29,12,1985), "Rua J, 412", "Nova Soure", "BA", "759939129", "murilo@gmail.com")
 ]

calcIdade :: DataAtual -> Cidadao -> IdadeCidadao
calcIdade dataAtual cidadao
   | (dia < diaAtual) && (mes < mesAtual) = anoAtual - ano
   | (mes < mesAtual) || (mes == mesAtual && dia < diaAtual) =(anoAtual - ano) - 1
   | otherwise = anoAtual - ano
   where 
     (dia, mes, ano) = alocDataNasc cidadao
     (diaAtual, mesAtual, anoAtual) = dataAtual

alocDataNasc :: Cidadao -> DataNasc
alocDataNasc (_, _, _, dataNasc, _, _, _, _, _) = dataNasc

-- Item (a)

adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS x xs =
  if checaCPF(pegaCPF x) xs
     then x:xs
     else error "CPF já está registrado"      

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF cpf cadastro = or [(pegaCPF cidadao) == cpf | cidadao <- dataBank]

-- Item (b)

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS cpf cadastro novoEnd =
 [alterarCidadao cpf item novoEnd | item <- cadastro]
  where
     alterarCidadao cpfProcurado (cpf, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) novoEnd =
       if cpfProcurado == cpf
         then (cpf, nome, genero, dataNasc, novoEnd, municipio, estado, telefone, email)
         else (cpf, nome, genero, dataNasc, endereco, municipio, estado, telefone, email)

atualizaTelSUS :: CPF -> CadastroSUS -> Telefone -> CadastroSUS
atualizaTelSUS cpf cadastro novoTel =
 [alterarCidadao cpf item novoTel | item <- cadastro]
  where
     alterarCidadao cpfProcurado (cpf, nome, genero, dataNasc, endereco, municipio, estado, telefone, email) novoTel =
       if cpfProcurado == cpf
         then (cpf, nome, genero, dataNasc, endereco, municipio, estado, novoTel, email)
         else (cpf, nome, genero, dataNasc, endereco, municipio, estado, telefone, email)

-- Item (c)

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpf dataBank 
   | not (checaCPF cpf dataBank) = error "CPF NAO ENCONTRADO"
   | otherwise = [cidadao | cidadao <- dataBank, cpf /= (pegaCPF cidadao)]

-- Item (d)

type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)

pegarMunicip :: Cidadao -> Municipio
pegarMunicip (_, _,_,_,_,muni,_,_,_) = muni

pegarEstado :: Cidadao -> Estado
pegarEstado (_,_,_,_,_,_,estad,_,_) = estad

pegarDataNasc :: Cidadao -> DataNasc
pegarDataNasc (_,_,_,nascim,_,_,_,_,_) = nascim

pegarIdade :: Cidadao -> Data -> Int
pegarIdade cidadao dataUtilizada    
    | (mesUsar < mesNasc) || (mesNasc == mesUsar && diaUsar < diaNasc) =  anoUsar - anoNasc - 1
    
    | otherwise = (anoUsar - anoNasc) 
    where (diaNasc, mesNasc, anoNasc) = pegarDataNasc cidadao
          (diaUsar, mesUsar, anoUsar) = dataUtilizada

checarIdadeNaFaixa :: Cidadao -> Data -> FaixaIdade -> Bool
checarIdadeNaFaixa cidadaos dataUtilizada (inicial, final) = (pegarIdade cidadaos dataUtilizada) >= inicial && (pegarIdade cidadaos dataUtilizada) <= final

cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorMunicipio  dataBank municip = 
  length [cidadaos | cidadaos <- dataBank, municip == (pegarMunicip cidadaos)]

cidadaosPorEstado :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorEstado dataBank estado =
  length [cidadao | cidadao <- dataBank, estado == (pegarEstado cidadao)]  

cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio -> Data -> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade dataBank municip dataUtilizada (inicial, final) = length [ cidadaos | cidadaos <- dataBank, municip == (pegarMunicip cidadaos),checarIdadeNaFaixa cidadaos dataUtilizada (inicial, final)]

cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> Data -> FaixaIdade -> Quantidade
cidadaosPorEstadoIdade dataBank estad dataUtilizada (inicial, final) = length [ cidadaos | cidadaos <- dataBank, estad == (pegarMunicip cidadaos),checarIdadeNaFaixa cidadaos dataUtilizada (inicial, final)]

-- Item (e)

listaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> IO()
listaMunicipioFaixas dataBank muni dataUtilizada listaFaixas = putStrLn ("MUNICIPIO: " ++ muni ++ "\nFAIXAS IDADES     QUANTIDADE\n" ++ (formataLinhas (geraListaMunicipioFaixas dataBank muni dataUtilizada listaFaixas)) ++ formataTotal (geraListaMunicipioFaixas dataBank muni dataUtilizada listaFaixas))

listaEstadoFaixas :: CadastroSUS -> Estado -> Data -> [FaixaIdade] -> IO()
listaEstadoFaixas dataBank est dataUtilizada listaFaixas = 
  putStrLn ("ESTADO: " ++ est ++ "\nFAIXAS IDADES     QUANTIDADE\n" ++ (formataLinhas (geraListaEstadoFaixas dataBank est dataUtilizada listaFaixas)) ++ formataTotal (geraListaEstadoFaixas dataBank est dataUtilizada listaFaixas))

geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas dataBank mun dataUtilizada listaFaixas =
     [(faixasIdade, qtd) | faixasIdade <- listaFaixas, qtd <- [cidadaosPorMunicipioIdade dataBank mun dataUtilizada faixasIdade]] 

geraListaEstadoFaixas :: CadastroSUS -> Estado -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaEstadoFaixas dataBank est dataUtilizada listaFaixas =
     [(faixasIdade, qtd) | faixasIdade <- listaFaixas, qtd <- [cidadaosPorEstadoIdade dataBank est dataUtilizada faixasIdade]]

-- Item (f)

-- Requisito 1

type Quantidade = Int
type QuantidadeFormatada = String
formataQuant :: Quantidade -> QuantidadeFormatada
formataQuant qtd = "          " ++ (show qtd)

--Requisito 2

type LinhaFormatada = String
formataUmaLinha :: (FaixaIdade, Quantidade)-> LinhaFormatada
formataUmaLinha (faixasIdade, qtd) = 
  (show (fst faixasIdade)) ++ " - " ++ (show (snd faixasIdade)) ++ formataQuant qtd

--Requisito 3

type LinhasFormatadas = String
formataLinhas :: [(FaixaIdade, Quantidade)] -> LinhasFormatadas
formataLinhas  listaFaixasQtd = concat [(formataUmaLinha faixaIdQtd) ++ "\n" | faixaIdQtd <- listaFaixasQtd]

--Requisito 4

type TotalFormatado = String
formataTotal :: [(FaixaIdade,Quantidade)] -> TotalFormatado
formataTotal listaFaixasQtd =
  "\nTOTAL            " ++ (show (encontraTotal listaFaixasQtd))

encontraTotal :: [(FaixaIdade,Quantidade)] -> Quantidade
encontraTotal listaFaixasQtd = 
  sum [qtd | (faixasIdade, qtd) <- listaFaixasQtd]

-- Item (g)

type Vacinados = [Vacinado]

dataBankVacinados :: Vacinados
dataBankVacinados = 
 [ 
 ( 3, [("Pfizer", (1, 12, 2020)), 
 ("Pfizer", (30, 11, 2020))] ),
 (10, [ ("Moderna", (2, 11, 2020)) ])
 ]

type Vacina = String
type TipoDose = Int
type Dose = (Vacina, Data)
type Doses = [Dose]
type Vacinado = (CPF, Doses)

pegaCPF :: Cidadao -> CPF
pegaCPF (cpf, _, _, _, _, _, _, _, _) = cpf

pegaCidadao :: CPF -> CadastroSUS -> CadastroSUS
pegaCidadao cpf dataBank =
  [cidadao | cidadao <- dataBank, cpf == (pegaCPF cidadao)]

pegaDosesDeVacinado :: Vacinado -> Doses
pegaDosesDeVacinado (_, dosesDoVacinado) = dosesDoVacinado

pegaCPFDeVacinado :: Vacinado -> CPF
pegaCPFDeVacinado (cpfVacinado,  _)  = cpfVacinado

pegaDataDaDose :: Vacinado -> Data
pegaDataDaDose ( _,     [(_, (date))]     ) = date

pegaAnoDaDose :: Vacinado -> Int
pegaAnoDaDose ( _,     [(_, ( _, _, anoData))]     ) = anoData

pegaMesDaDose :: Vacinado -> Int
pegaMesDaDose ( _,     [(_, ( _, mesData, _))]     ) = mesData

pegaDiaDaDose :: Vacinado -> Int
pegaDiaDaDose ( _,     [(_, ( diaData, _, _))]     ) = diaData

pegaAnoDaData :: Data -> Int
pegaAnoDaData ( _, _, anoData)      = anoData

pegaMesDaData :: Data -> Int
pegaMesDaData ( _, mesData, _)      = mesData

pegaDiaDaData :: Data -> Int
pegaDiaDaData ( diaData, _, _)      = diaData

-- Funções Auxiliares 

checaPrimDose :: CPF -> Vacinados -> Bool
checaPrimDose cpf dataBankVacinados = or [(pegaCPFDeVacinado cidadao) == cpf | cidadao <- dataBankVacinados]

checaIntervaloDeIdades :: CPF -> CadastroSUS -> FaixaIdade -> Data -> Bool
checaIntervaloDeIdades cpf dataBank (inicial, final) dat =
  length[cidadao | cidadao <- dataBank, cpf == (pegaCPF cidadao), inicial <= pegarIdade cidadao dat && pegarIdade cidadao dat <= final] == length((pegaCidadao cpf dataBank))

primDoseAplicada :: CPF -> Vacinados -> Bool
primDoseAplicada cidadaoCPF dataBankVacinados = 
    length [pessoaData | pessoaData <- dataBankVacinados, cidadaoCPF == (pegaCPFDeVacinado pessoaData)] /= 0

checaMunicipioDoCidadao :: CPF -> CadastroSUS -> Municipio -> Bool
checaMunicipioDoCidadao cpf cadastro mun =
 length[cidadao | cidadao <- dataBank, cpf == (pegaCPF cidadao), (pegarMunicip cidadao) == mun] == (length(pegaCidadao cpf dataBank))
 
dosesJaTomadasPorCidadao :: CPF -> Vacinados -> Doses
dosesJaTomadasPorCidadao cpf dataBankVacinados = 
         head [vacinasJaAplicadas | (cpf, vacinasJaAplicadas) <- dataBankVacinados, cpf == cpf]

contadorDeVacinasJaTomadasPorCidadao :: CPF -> Vacinados -> Int
contadorDeVacinasJaTomadasPorCidadao  cpf dataBankVacinados = 
  length (dosesJaTomadasPorCidadao cpf dataBankVacinados)

checadorSeDataSegDoseMaiorQuePrimDose :: CPF -> Data -> Vacinados -> Bool
checadorSeDataSegDoseMaiorQuePrimDose cpf dataVacinacao dataBankVacinados = 
        (length [vacinado | vacinado <- dataBankVacinados, cpf == pegaCPFDeVacinado vacinado, fComparadorDeData dataVacinacao vacinado]) /= 0
          where 
              fComparadorDeData (diaDaNovaVacina, mesDaNovaVacina, anoDaNovaVacina) ( cpfData, [(vacinaData, (diaData, mesData, anoData))] )
                | anoData < anoDaNovaVacina                                                                        = True
                | anoData == anoDaNovaVacina && mesData < mesDaNovaVacina                                          = True
                | anoData == anoDaNovaVacina && mesData == mesDaNovaVacina && diaData < diaDaNovaVacina            = True
                | otherwise                                                                                        = False

adicionaOutraDoseAoVacinado :: CPF -> Data -> Vacinados -> Vacinados
adicionaOutraDoseAoVacinado cpf dataVacinacao dataBankVacinados = 
    concat [  [fAdicionarVacina (cpfData,[(vacinaData,dataVacinacaoData)]) cpf | (cpfData,[(vacinaData,dataVacinacaoData)]) <- dataBankVacinados], [vacinado | vacinado <- dataBankVacinados, not (cpf == pegaCPFDeVacinado vacinado)] ]
             where
               fAdicionarVacina (cpfData,[(vacinaData,dataVacinacaoData)]) cpf
                 | (cpfData == cpf)                            =  (cpf,(:) (vacinaData, dataVacinacao) [ (vacinaData,dataVacinacaoData)])     

checaCPFBancoDeVacinas :: CPF -> Vacinados -> Bool
checaCPFBancoDeVacinas cpf dataBankVacinados =  
    length [vaccinated | vaccinated <- dataBankVacinados, cpf == pegaCPFDeVacinado vaccinated] /= 0

fAtualizaVacina :: CPF -> TipoDose -> Vacina -> Vacinados -> Vacinados
fAtualizaVacina cpf tipodose vacinaAlterada dataBankVacinados =
    concat [ [falterarVacina tipodose vacinaAlterada vacinadoAlterado | vacinadoAlterado <- dataBankVacinados, cpf == pegaCPFDeVacinado vacinadoAlterado], [vacinado | vacinado <- dataBankVacinados, not (cpf == pegaCPFDeVacinado vacinado)]  ]
      where
         falterarVacina tipodose vacinaAlterada vacinadoAlterado
          | length (pegaDosesDeVacinado vacinadoAlterado) == 2   = falterarvacinacom2Doses tipodose vacinaAlterada vacinadoAlterado 
          | length (pegaDosesDeVacinado vacinadoAlterado) == 1   = falterarvacinacom1Dose  vacinaAlterada vacinadoAlterado 

falterarvacinacom1Dose :: Vacina -> Vacinado -> Vacinado 
falterarvacinacom1Dose vacinaAlterada (cpfData,[(vacinaData,dataVacinacaoData)]) =
    (cpfData,[(vacinaAlterada,dataVacinacaoData)]) 

falterarvacinacom2Doses :: TipoDose -> Vacina -> Vacinado -> Vacinado 
falterarvacinacom2Doses tipodose vacinaAlterada (cpfData, [(vacina1,dataVacinacao1), (vacinaData2,dataVacinacao2)] )
   | tipodose == 1    = (cpfData, [(vacinaAlterada, dataVacinacao1), (vacinaData2,dataVacinacao2)]) 
   | tipodose == 2    = (cpfData, [(vacina1,dataVacinacao1), (vacinaAlterada,dataVacinacao2)])      

aplicaPrimDose :: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Data -> Vacinados -> Vacinados
aplicaPrimDose cpf dataBankSUS faixadeidade municipio vacina dataVacinacao dataAtual dataBankVacinados
  | (primDoseAplicada cpf dataBankVacinados == True)                          = error "Primeira dose já aplicada"  
  | (checaCPF cpf dataBankSUS == False)                                     = error "CPF não encontrado no Banco de Dados SUS" 
  | (checaIntervaloDeIdades cpf dataBankSUS faixadeidade dataAtual == False) = error "Fora da idade de vacinação corrente" 
  | (checaMunicipioDoCidadao cpf dataBankSUS municipio == False)            = error "Municipio não compatível com o CadastroSUS. Por favor atualizar município." 
  | vacina == "Jansen"                                 = (:) (cpf, [(vacina, dataVacinacao), (vacina, dataVacinacao)])  dataBankVacinados 
  | otherwise                                          = (:) (cpf, [(vacina, dataVacinacao)]) dataBankVacinados 

-- Item (h)

aplicaSegDose :: CPF -> Data -> Vacinados -> Vacinados
aplicaSegDose cpf dataVacinacao dataBankVacinados 
  | (primDoseAplicada cpf dataBankVacinados == False)                                      = error "Primeira dose não tomada pelo cidadão." 
  | (contadorDeVacinasJaTomadasPorCidadao cpf dataBankVacinados == 2)                      = error "Segunda dose já tomada pelo cidadão." 
  | (checadorSeDataSegDoseMaiorQuePrimDose cpf dataVacinacao dataBankVacinados == False)   = error "Data Inválida, por favor corrigir." 
  | otherwise                                                                = adicionaOutraDoseAoVacinado cpf dataVacinacao dataBankVacinados 

-- Item (i)

atualizaVacina:: CPF -> TipoDose -> Vacina -> Vacinados -> Vacinados
atualizaVacina cpf tipodose vacina dataBankVacinados
 | (checaCPFBancoDeVacinas cpf dataBankVacinados == False)                  = error "CPF não cadastrado." 
 | (contadorDeVacinasJaTomadasPorCidadao cpf dataBankVacinados) < tipodose  = error "O cidadão ainda nã tomou esta dose." 
 | otherwise                                                  = fAtualizaVacina cpf tipodose vacina dataBankVacinados 

pegaCidadao2 :: CPF -> CadastroSUS -> Cidadao
pegaCidadao2 cpf dataBankSUS =
    head [(cpf, nome, gen, nasc, end, mun, estado, tel, email) | (cpf, nome, gen, nasc, end, mun, estado, tel, email) <- dataBankSUS, cpf ==  cpf]

pegarMunicip2 :: CPF -> CadastroSUS -> Municipio
pegarMunicip2 cpf dataBankSUS = pegarMunicip (pegaCidadao2 cpf dataBankSUS)

pegarEstado2 :: CPF -> CadastroSUS -> Estado
pegarEstado2 cpf dataBankSUS = pegarEstado (pegaCidadao2 cpf dataBankSUS)

-- Item (j)

quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun dataBankVacinados tipodose municipio dataBankSUS =
    length [cidadao | cidadao <- dataBankVacinados,  (contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados) >= tipodose, pegarMunicip2 (fst cidadao) dataBankSUS == municipio]

quantidadeDoseEst :: Vacinados -> TipoDose -> Estado -> CadastroSUS -> Quantidade
quantidadeDoseEst dataBankVacinados tipodose estado dataBankSUS =
    length [cidadao | cidadao <- dataBankVacinados,  (contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados) >= tipodose, pegarEstado2 (fst cidadao) dataBankSUS == estado]

-- Item (k)

quantidadeMunIdDose :: Vacinados -> Municipio -> FaixaIdade -> TipoDose -> Data -> CadastroSUS -> Quantidade
quantidadeMunIdDose dataBankVacinados municipio (ageInit, ageEnd) tipodose dataAtual dataBankSUS =
    length [cidadao | cidadao <- dataBankVacinados,  (contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados) >= tipodose, pegarMunicip2 (fst cidadao) dataBankSUS == municipio, checaIntervaloDeIdades (fst cidadao) dataBankSUS (ageInit, ageEnd) dataAtual]

quantidadeEstIdDose :: Vacinados -> Estado -> FaixaIdade -> TipoDose -> Data -> CadastroSUS -> Quantidade
quantidadeEstIdDose dataBankVacinados estado (ageInit, ageEnd) tipodose dataAtual dataBankSUS =
    length [cidadao | cidadao <- dataBankVacinados,  (contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados) >= tipodose, pegarEstado2 (fst cidadao) dataBankSUS == estado, checaIntervaloDeIdades (fst cidadao) dataBankSUS (ageInit, ageEnd) dataAtual]

-- -- Item (l)

quantidadeMunVacDose :: Vacinados -> Municipio -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeMunVacDose dataBankVacinados municipio vacina tipodose dataBankSUS =
    length [cidadao | cidadao <- dataBankVacinados,  (contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados) >= tipodose, pegarMunicip2 (fst cidadao) dataBankSUS == municipio, fSeEhVacinaQueProcuro cidadao vacina dataBankVacinados]
      where
          fSeEhVacinaQueProcuro cidadao vacina dataBankVacinados
             | contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados == 2    = fChecadorDeDuasDoses vacina cidadao
             | contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados == 1    = fChecadorDeUmaDose vacina cidadao

quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstVacDose dataBankVacinados estado vacina tipodose dataBankSUS =
    length [cidadao | cidadao <- dataBankVacinados,  (contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados) >= tipodose, pegarEstado2 (fst cidadao) dataBankSUS == estado, fSeEhVacinaQueProcuro cidadao vacina dataBankVacinados]
      where
          fSeEhVacinaQueProcuro cidadao vacina dataBankVacinados
             | contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados == 2    = fChecadorDeDuasDoses vacina cidadao
             | contadorDeVacinasJaTomadasPorCidadao  (fst cidadao) dataBankVacinados == 1    = fChecadorDeUmaDose vacina cidadao

fChecadorDeDuasDoses :: Vacina -> Vacinado -> Bool
fChecadorDeDuasDoses vacina (cpfData, [(vacina1,dataVacinacao1), (vacinaData2,dataVacinacao2)])
    | vacina1 == vacina                         = True
    | otherwise                                 = False

fChecadorDeUmaDose :: Vacina -> Vacinado -> Bool
fChecadorDeUmaDose vacina (cpfData, [(vacina1,dataVacinacao1)])
   | vacina == vacina1                          = True
