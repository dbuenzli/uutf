open Assemblage 

let unix = `Pkg "unix"
let uutf = lib [ cu ~dir:"src" [] "uutf" ] "uutf"

let utftrip = 
  bin ~install:true [ cu ~dir:"test" [ uutf; unix ] "utftrip" ] "utftrip"

let test = 
  let t = bin ~install:false [ cu ~dir:"test" [ uutf ] "test" ] "test" in 
  test [] [(test_bin t (fun _ -> []))] "test"

let examples = 
  bin ~install:false [ cu ~dir:"test" [ uutf; unix ] "examples" ] "examples"
                                      
let () = create [ uutf; utftrip; test; examples ] "uutf"
