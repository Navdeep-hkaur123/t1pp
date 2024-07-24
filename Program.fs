// For more information see https://aka.ms/fsharp-console-apps

let rec showSalaries list =
    match list with
    | [] -> ()
    | first :: rest ->
        printfn "%d" first
        showSalaries rest

let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]
printfn "Here are the salaries:"
showSalaries salaries

let highIncomeSalaries = List.filter (fun salary -> salary > 100000) salaries
printfn "High-income salaries: %A" highIncomeSalaries

let calculateTax salary =
    let salaryAsFloat = float salary
    let taxRate =
        match salaryAsFloat with
        | s when s <= 49020.0 -> 0.15
        | s when s <= 98040.0 -> 0.205
        | s when s <= 151978.0 -> 0.26
        | s when s <= 216511.0 -> 0.29
        | _ -> 0.33
    salaryAsFloat * taxRate
    
let taxedSalaries = salaries |> List.map calculateTax
printfn "Taxed salaries: %A" taxedSalaries

let filteredSalaries = List.filter (fun salary -> salary < 49020) salaries
printfn "Salaries less than $49,020: %A" filteredSalaries

let updatedSalaries = 
    salaries 
    |> List.map (fun salary -> if salary < 49020 then salary + 20000 else salary)
printfn "Updated salaries: %A" updatedSalaries

let midRangeSalaries = List.filter (fun salary -> salary >= 50000 && salary <= 100000) salaries
printfn "Filtered salaries between $50,000 and $100,000: %A" midRangeSalaries

let sumMidRangeSalaries = List.fold (+) 0 midRangeSalaries
printfn "Sum of mid-range salaries: %d" sumMidRangeSalaries


