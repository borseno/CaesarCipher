// dotnet run key=value1 text=value2 

open System
open Microsoft.Extensions.Configuration

let table = ['а';'б';'в';'г';'д';'е';'ё';'ж';'з';'и';'й';'к';'л';'м';'н';'о';'п';'р';'с';'т';'у';'ф';'х';'ц';'ч';'ш';'щ';'ъ';'ы';'ь';'э';'ю';'я'] 

let charIndex char = 
    List.tryFindIndex (fun i -> Char.ToUpper char = Char.ToUpper i) table

let encryptCharacter func keyChar textChar = 
                    let keyIndex, textIndex = charIndex keyChar, charIndex textChar
                    match textIndex with
                    | None -> None
                    | Some index -> Some(table.[ func table.Length keyIndex.Value index])

let caesar func (key:string) (text:string) =
    seq {
        let mutable keyIndex = 0
        for ch in text do         
          match ( encryptCharacter func key.[keyIndex % key.Length] ch ) with
          | Some result -> 
            keyIndex <- keyIndex + 1
            result
          | None -> ch  
    } |> Seq.toArray |> String

let encryption n k x = (x + k) % n
let decryption n k y = ( (y + n) - (k % n) ) % n

let encryptCaesar = caesar encryption
let decryptCaesar = caesar decryption

[<EntryPoint>]
let main argv =
    let config = ConfigurationBuilder().AddCommandLine(argv).Build()
    Console.OutputEncoding <- System.Text.Encoding.Unicode
    printfn "%s" (decryptCaesar config.["key"] config.["text"])
    Console.ReadKey() |> ignore
    0 // return an integer exit code