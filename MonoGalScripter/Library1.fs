namespace MonoGalScripterFSharp
open System
open System.Text
open System.IO
open FSharp.Linq
open Microsoft.FSharp.Compiler.Interactive.Shell
//open Yaaf.FSharp.Scripting
open System.Collections.Generic
type ImpRef =
    { 
        Path:string;
        Type:string;
        Loaded:bool;
        Reference:string;
    }
type GalScript = 
    {
        Content:string[];
        Name:string;
    }
type ScriptLabel = 
    {
        FileName:string;
        StartRef:int;
        EndRef:int;
    }

module Scripter = //build an embedded interactive
// Intialize output and input streams



        //let fsiSession = ScriptHost.CreateNew()
        let eval text = //helper functions

// Intialize output and input streams
            let sbOut = new StringBuilder()
            let sbErr = new StringBuilder()
            let inStream = new StringReader("")
            let outStream = new StringWriter(sbOut)
            let errStream = new StringWriter(sbErr)

// Build command line arguments & start FSI session
            let argv = [| "fsi.exe" |]
            let allArgs = Array.append argv [|"--noninteractive"|]

            let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
            let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)  


            match fsiSession.EvalExpression(text) with
            | Some value->value.ReflectionValue
            | None ->null
            //match fsiSession.EvalExpression(text) with//let temp=a:?>B.FSharpFullNameWithArgs
            //| Some value -> value
            //| None -> null,null
        //let execute text =
            //fsiSession.EvalInteraction text
        //let evalScript filePath = 
            //fsiSession.EvalScript filePath
        //let constDict = new Dictionary<string,obj*Type>()
        //let varDict = new Dictionary<string,obj*Type>()
       // let Let name constObj =
             //fsiSession.Let name constObj
             //constDict.Add(name, constObj)
        //let Var name varObj =
            //fsiSession.Let "varTemp" varObj
            //let arg="let mutable "+ name + " = varTemp"
            //execute arg
module Evaluator =
        let readNextWord (text:string) = //helper methods
            let makeString (array:Char[]) = new string(array)
            let charArray=
                text.ToCharArray()
                |>Seq.skipWhile(Char.IsWhiteSpace)
            let nextWord=
                charArray
                |>Seq.takeWhile(fun x->not(Char.IsWhiteSpace(x)))
                |>Seq.toArray
                |>makeString
            let remainingText=
                (makeString(Seq.toArray(charArray))).Substring(nextWord.Length).ToCharArray()
                |>Seq.skipWhile(Char.IsWhiteSpace)
                |>Seq.toArray
                |>makeString
            nextWord, remainingText
        let getRefData (text:string) = //reference
            let ref=text.Split([|'-'|])
            ref.[0], Convert.ToInt32(ref.[1])
        let encodeRef fileName lineNum = 
            fileName+"-"+lineNum
        let readImportRef (script:string[]) = //Read import references
            let importList =
                script
                |>Array.filter(fun x->(fst(readNextWord x))="import")
                |>Array.map(fun x->snd(readNextWord(x)))
            let assemblyList= //dll assemblies
                importList
                |>Array.filter(fun x->fst(readNextWord x)="assembly")
                |>Array.map(fun x->snd(readNextWord(x)))
            let externalList= //external fsharps
                importList
                |>Array.filter(fun x->fst(readNextWord x)="external")
                |>Array.map(fun x->snd(readNextWord(x)))
            let resList= //resources
                importList
                |>Array.filter(fun x->fst(readNextWord x)="res")
                |>Array.map(fun x->snd(readNextWord(x)))
            let scriptList= //scripts
                importList
                |>Array.filter(fun x->fst(readNextWord x)="script")
                |>Array.map(fun x->snd(readNextWord(x)))
            assemblyList, externalList, resList, scriptList
        let scanLabels (script:string[]) (fileName:string) = //Scan script labels using imperative programming
            let findIndention (line:string)= //find indention
                            let firstWord=fst(readNextWord(line))
                            line.IndexOf(firstWord)
            let labelDictionary=new Dictionary<string,ScriptLabel>()//Do it have to be mutable?
            for i=0 to script.Length do
                let firstWord, remainingText= readNextWord script.[i]
                if firstWord="label" then //if the line declares a label
                    let labelName=fst(readNextWord(remainingText)) //read label name
                    if i<script.Length-1 && findIndention script.[i]<findIndention script.[i+1] then //iter to find index of the end of the label block
                        let mutable index=i+1
                        let indention=findIndention script.[i+1]
                        while not(fst(readNextWord(script.[index]))="label") && findIndention script.[index]=indention do
                            index<-index+1
                        index<-index-1
                        let scriptLabel={FileName=fileName;StartRef=i;EndRef=index}
                        labelDictionary.Add(labelName, scriptLabel)
                    else
                        let scriptLabel={FileName=fileName;StartRef=i;EndRef=i}
                        labelDictionary.Add(labelName, scriptLabel)

           


