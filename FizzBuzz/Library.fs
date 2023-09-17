namespace FizzBuzz

[<RequireQualifiedAccess>]
module Option =

    /// <summary>
    /// Match any tuple that matches the signature of bool * a' to an a' option.
    /// When the result of 
    /// </summary>
    let fromTryTuple = function
        | false, _ -> None
        | true, x -> Some x

[<RequireQualifiedAccess>]
module Result =

    /// <summary>
    /// Convert an option into a result by providing an error value message when None.
    /// </summary>
    /// <param name="errorValue">An error value to pass to the result when None.</param>
    let fromOption errorValue = function
        | Some x -> Ok x
        | None -> Error errorValue

module Parser =

    open System

    /// <summary>Parse user input and try to convert it to an integer.</summary>
    /// <param name="input">The user input to try to parse.</param>
    /// <returns>An int option where the result is None if the value 
    /// cannot be parsed, or some for a successful parse.</returns>
    let tryParse (input: string) =
        Int32.TryParse input
        |> Option.fromTryTuple


module Validator =

    /// <summary>
    /// An integer that has been validated by the application already.
    /// </summary>
    type ValidNumber = 
        private ValidNumber of int

    module ValidNumber =
        /// <summary>
        /// Deconstruct a privately constructed ValidNumber object to get its value.
        /// </summary>
        /// <param name="number">The resulting number from deconstructing.</param>
        let value (ValidNumber number) = number 

        /// <summary>
        /// Validate that an input number is within the range 1..4000.
        /// </summary>
        /// <param name="number">The input number to validate.</param>
        let isValidNumber number =
            match 1 <= number && number <= 4000 with
            | false -> None
            | true -> Some <| ValidNumber number

module FizzBuzz =

    open Validator

    /// <summary>
    /// Given a number, create a list of strings that match the FizzBuzz rules.
    /// Rule 1: When 3 is a factor of the number, replace the number with Fizz
    /// Rule 2: When 5 is a factor of the number, replace the number with Buzz
    /// Rule 3: When both 3 and 5 are factors, replace the number with FizzBuzz
    /// Rule 4: In any other case, just return the number.
    /// </summary>
    /// <param name="number">The upper bound for the range of numbers. 
    /// FizzBuzz will be performed starting at 1.</param>
    let getFizzBuzzString (validNumber) =
        [1..ValidNumber.value validNumber]
        |> List.map (fun n -> (n, n % 3, n % 5))
        |> List.map (function
            | (_, 0, 0) -> "FizzBuzz"
            | (_, 0, _) -> "Fizz"
            | (_, _, 0) -> "Buzz"
            | (n, _, _) -> string n)
        |> String.concat "\n"

module Domain =

    open Validator

    type ParseNumber = string -> int option
    type ValidateNumber = int -> ValidNumber option
    type GetFizzBuzzString = ValidNumber -> string

    type ParserError = NotANumber of string
    type ValidatorError = InvalidNumber of int

    type Error =
        | ParserError of ParserError
        | ValidatorError of ValidatorError

    type ExecuteFizzBuzzWorkflow = string -> Result<string, Error>

    let execute (parseNumber: ParseNumber) (validateNumber: ValidateNumber) (getFizzBuzzString: GetFizzBuzzString) : ExecuteFizzBuzzWorkflow =
        
        let parseNumber' input = 
            input
            |> parseNumber
            |> Result.fromOption (NotANumber input)
            |> Result.mapError ParserError

        let validateNumber' number =
            number
            |> validateNumber
            |> Result.fromOption (InvalidNumber number)
            |> Result.mapError ValidatorError
        
        fun input -> 
            input
            |> parseNumber'
            |> Result.bind validateNumber'
            |> Result.map getFizzBuzzString

module Application =

    open Domain

    type Input = unit -> string
    type Output = string -> unit

    let execute =
        Domain.execute
            Parser.tryParse
            Validator.ValidNumber.isValidNumber
            FizzBuzz.getFizzBuzzString

    let application (input: Input) (output: Output) =
        fun () ->
            output "Please enter a number between 1 and 4000:"
            input ()
            |> execute
            |> function
                | Ok s -> 
                    sprintf "Here is the output:\n%s" s
                    |> output
                | Error (ParserError (NotANumber s)) -> 
                    sprintf "%s is not an integer" s
                    |> output
                | Error (ValidatorError (InvalidNumber num)) ->
                    sprintf "You entered %i. Please enter a valid integer between 1 and 4000." num
                    |> output

        