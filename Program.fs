// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.RegularExpressions

// Create a function that will capitalize first letter of a string
let capitalizeFirstLetter (s: string) =
    s |> Seq.mapi (fun i c -> match i with | 0 -> (Char.ToUpper(c)) | _ -> c)  |> String.Concat

// Create a function that will determine if a string is a number
let strContainsOnlyNumber (s:string) =
    s |> Seq.forall Char.IsDigit

// Create a function that will determine if a string is a number
let strContainsOnlyLetter (s:string) =
    s |> Seq.forall Char.IsLetter

let strip chars = String.collect (fun c -> if Seq.exists((=)c) chars then "" else string c)

// Create a function that will count all the words in a string
let count(text : string) =
  let words = text.Split [|' '|]
  let nWords = words.Length
  (nWords)

let reverseList list = list |> List.rev

// Create a function that will capitalize letters after a dash and apostrophe.
let capitalizeAfterOccurence(text : string) =
    let mutable stringBuild = ""
    let mutable input = text
    let mutable characterList = []
    let mutable counter = 0

    for element in input do

        if element = '-' || element = ''' then
            counter <- counter + 1
            characterList <- element :: characterList

        else
            if counter = 1 then
                let mutable x = Char.ToUpper element
                characterList <- x :: characterList 
                counter <- 0
            else
                characterList <- element :: characterList
    done

    characterList <- reverseList characterList
    stringBuild <- List.fold (fun str x -> str + x.ToString()) "" (List.ofSeq characterList) 

    (stringBuild)

let convertDateToWord(date : string) =
    // Declare an empty list to build date string
    let mutable dateBuilder = []

    // Declare month, day, and year variables
    let month : string = date.[0..1]
    let mutable day : string = date.[3..4]
    let year : string = date.[6..]

    if month = "01" then
        dateBuilder <- "January" :: dateBuilder
    else if month = "02" then
        dateBuilder <- "February" :: dateBuilder
    else if month = "03" then
        dateBuilder <- "March" :: dateBuilder
    else if month = "04" then
        dateBuilder <- "April" :: dateBuilder
    else if month = "05" then
        dateBuilder <- "May" :: dateBuilder
    else if month = "06" then
        dateBuilder <- "June" :: dateBuilder
    else if month = "07" then
        dateBuilder <- "July" :: dateBuilder
    else if month = "08" then
        dateBuilder <- "August" :: dateBuilder
    else if month = "09" then
        dateBuilder <- "September" :: dateBuilder
    else if month = "10" then
        dateBuilder <- "October" :: dateBuilder
    else if month = "11" then
        dateBuilder <- "November" :: dateBuilder
    else
        dateBuilder <- "December" :: dateBuilder

    // Add the day to the dateBuilder list
    day <- day + ","
    dateBuilder <- day :: dateBuilder

    // Add the year to the dateBuilder list
    dateBuilder <- year :: dateBuilder

    // Reverse the order of the list
    dateBuilder <- reverseList dateBuilder

    // Build the string using concatenation
    let finalDate = String.concat " " dateBuilder

    // Return the value within the finalDate value
    (finalDate)

// Create a function that determines if a string contains spaces.
let containsSpace(input : string) =
    // Break down the input string into a character array.
    let inputArr : char[] = input.ToCharArray()
    // Declare a variable as a bool.
    let mutable hasSpacing : int32 = 0

    // Iterate throughout the inputArr.
    for character in inputArr do
        // Determine if a character is a space.
        if character = ' ' then
            // If true, set hasSpacing to true.
            hasSpacing <- hasSpacing + 1
    done
    // Return the value within the hasSpacing variable.
    (hasSpacing)

// Create a function that finds out if a string contains Letters and Numbers.
let strContainsLettersAndNumbers(input : string) =
    // Break down the input string into a character array.
    let inputArr : char[] = input.ToCharArray()
    // Set to False, until proven otherwise.
    let mutable isLetterNumber : bool = false
    let mutable hasNumber : bool = false
    let mutable hasLetter : bool = false

    // Iterate throughout the character array.
    for character in inputArr do
        // Find out if the character is a digit.
        if Char.IsDigit(character) then
            // Set hasNumber to true.
            hasNumber <- true
        // Find ot if the character is a letter.
        else if Char.IsLetter(character) then
            // Set hasLetter to true.
            hasLetter <- true
    done

    // If hasNumber and hasLetter is true set isLetterNumber to true.
    if hasNumber = true && hasLetter = true then
        isLetterNumber <- true
    else
        // Set isLetterNumber to false
        isLetterNumber <- false

    (isLetterNumber)
    
// Create a class called 'Student'
type Student(aForename : string, aSurname : string, aAge : int32, aDOB : string, aID : string, aEmail : string) =
    member this.Forename = aForename
    member this.Surname = aSurname
    member this.Age = aAge
    member this.DOB = aDOB
    member this.ID = aID
    member this.Email = aEmail
    override this.ToString()= sprintf "\n(%s, %s, %d, %s, %s, %s)" aForename aSurname aAge aDOB aID aEmail

let getForename() =
    let mutable continueLooping = true
    let mutable forename : string = ""
    while continueLooping do
        // Ask the user for the student name
        Console.WriteLine("\nPlease enter student forename: ")
        forename <- Console.ReadLine()

        // Get rid of the trailing whitespaces.
        forename <- forename.Trim()
        // Convert all characters within forename input to all lowercase.
        forename <- forename.ToLower()
        // Utilize capitalizeAfterOccurence function to capitalize names properly.
        forename <- capitalizeAfterOccurence(forename)

        // Utilize capitalizeFirstLetter function to capitalize first letter of input.
        let mutable updatedName =
            forename
            |> capitalizeFirstLetter

        // In case, the user name has any periods, dashes or apostrophes...
        // utilize this function
        updatedName <- capitalizeAfterOccurence(updatedName)

        // Determine if the input matches with the regular expression for the name
        if Regex.IsMatch(updatedName, "^[A-Z]([a-z]{1,32}((['.-]{1}([A-Z]{1}))?))+$") then
            forename <- updatedName
            printfn "[+] '%s' has been accepted..." forename
            continueLooping <- false
        // Determine if the input was empty
        else if(String.IsNullOrEmpty forename) then
            printfn "[*] Empty entries are not accepable..."
            continueLooping <- true
        // Find out if the input contains a space.
        else if containsSpace(forename) > 0 then
            printfn "[*] Forename should not include spaces in between lettering."
            continueLooping <- true
        // Determine if the forename input contains letters and numbers.
        else if strContainsLettersAndNumbers(forename) = true then
            printfn "[*] Names are not composed of letters and numbers"
            continueLooping <- true
        else
            // Determine if the input entered was fully a number
            if strContainsOnlyNumber(forename) then
                printfn "[*] Numbers cannot be considered as a name"

            else if(String.length forename = 1) then
                printfn "[*] A name cannot be one letter of length"

            else
                // Notify the user that the name is invalid
                printfn "[*] '%s' is not a valid name\n" forename

            // Continue iterating throughout the loops
            continueLooping <- true
    done

    // Return the value within the forename value.
    (forename)

let getSurname() =
    let mutable continueLooping = true
    let mutable surname : string = ""
    while continueLooping do
        // Ask the user for the student name
        Console.WriteLine("\nPlease enter student surname: ")
        surname <- Console.ReadLine()

        // Get rid of the trailing whitespaces.
        surname <- surname.Trim()
        // Convert all characters in input to lowercase
        surname <- surname.ToLower()
        // Utilize capitalizeAfterOccurence function to capitalize names properly.
        surname <- capitalizeAfterOccurence(surname)

        // Utilize capitalizeFirstLetter function to capitalize first letter of input
        let mutable updatedSurname =
            surname
            |> capitalizeFirstLetter

        // In case, the user name has any periods, dashes or apostrophes...
        // utilize this function
        updatedSurname <- capitalizeAfterOccurence(updatedSurname)

        // Determine if the input matches with the regular expression for the name
        if Regex.IsMatch( updatedSurname, "^[A-Z]([a-z]{1,32}((['.-]{1}([A-Z]{1}))?))+$" ) then
            surname <- updatedSurname
            printfn "[+] '%s' has been accepted..." surname
            continueLooping <- false
        else
            // Determine if the input was empty
            if(String.IsNullOrEmpty surname) then
                printfn "[*] Empty entries are not accepable..."
 
            // Determine if the input entered was fully a number
            else if strContainsOnlyNumber(surname) then
                printfn "[*] Numbers cannot be considered as a name."

            // Determine if the name is one letter of length.
            else if(String.length surname = 1 && strContainsOnlyLetter(surname)) then
                printfn "[*] A name cannot be one letter of length."

            // Find out if the input contains a space.
            else if containsSpace(surname) > 0 then
                printfn "[*] Surname should not include spaces in between lettering."

            // Determine if the forename input contains letters and numbers.
            else if strContainsLettersAndNumbers(surname) = true then
                printfn "[*] Names are not composed of letters and numbers"

            else
                // Notify the user that the name is invalid
                printfn "[*] '%s' is not a valid surname\n" surname

            // Continue iterating throughout the loops
            continueLooping <- true
    done

    // Return the surname value
    (surname)

// Create a function that will display the format rules for a UoR ID
let displayIDFormat() =
    printfn "\n------UoR Student ID Format------"
    printfn "[*] Begins with three capital letters"
    printfn "[*] Followed by eight digits (no spaces)"
    printfn "[*] Should correspond to first 3 letters of surname"
    printfn "[*] (ID consists of eleven characters in total)"

let getID(surname : string) =
    // Declare variables for usage
    let mutable continueLooping = true
    let mutable id : string = ""
    // Construct a while loop for validation purposes
    while continueLooping do

        // Display the UoR ID format by utilizing the displayIDFormat() function
        displayIDFormat()

        // Ask the user to enter the student ID.
        Console.WriteLine("\nPlease enter student ID (UoR format): ")
        id <- Console.ReadLine()

        // Get rid of the trailing whitespaces.
        id <- id.Trim()

        // Declare dash and apostrophe as variables (if there are any).
        let dashChar : string = "-"
        let apostropheChar : string = "'"
        let periodChar : string = "."

        // IDs must correlate to the first three letters of student surname.
        let mutable firstThreeSurname : string = ""

        // Remove dashes, periods and apostrophes from the surname.
        firstThreeSurname <- strip dashChar surname
        firstThreeSurname <- strip apostropheChar firstThreeSurname
        firstThreeSurname <- strip periodChar firstThreeSurname

        // Extract the first three letters of the surname.
        firstThreeSurname <- firstThreeSurname.[0..2]
        // Capitalize the first three letters of surname.
        firstThreeSurname <- firstThreeSurname.ToUpper()

        // Capitalize the first three characters of the string.
        let mutable prt1ID = id.[0..2]
        prt1ID <- prt1ID.ToUpper()
        // Declare a variable that represents all characters after the first three.
        let mutable prt2ID = id.[3..]
        
        // Concatenate both parts together to assemble a UoR formatted ID.
        id <- prt1ID + prt2ID

        // Determine if the input matches with the regular expression for the name.
        if Regex.IsMatch(id, "^[A-Z]{3}([0-9]{8})$") && firstThreeSurname = prt1ID then
            printfn "[+] '%s' has been accepted..." id
            continueLooping <- false

        // Determine if the input was empty.
        else if(String.IsNullOrEmpty id) then
            printfn "[*] Empty entries are not accepable..."
            continueLooping <- true
        else
            // Find out if the first three characters corresponded to the first...
            // three letters of the student surname.
            if(id.[0..2] <> firstThreeSurname) then
                printfn "[*] First 3 characters must correlate to the first 3 letters of surname."
                printfn "[*] Required usuage: %s (%s)" firstThreeSurname surname

            // Determine if the input only contained numbers.
            if(strContainsOnlyNumber(id)) then
                printfn "[*] UoR IDs are not fully comprised of numbers."

            // Determine if the input only contained letters.
            if(strContainsOnlyLetter(id)) then
                printfn "[*] UoR IDs are not fully comprised of letters."

            // Determine if the input was less than 11 characters long.
            let inputLength = String.length id
            if(String.length id <> 11) then
                // Add a message if the input was one character of length.
                if(inputLength = 1) then
                    printfn "[*] IDs are not one character of length."
                else 
                    // Display a message upon the short length of the input.
                    printfn "[*] ID input is %d characters long (UoR ID = 11 characters)" inputLength
      
            // Determine if the dob input contains space(s).
            if containsSpace(id) <> 0 then
                if containsSpace(id) = 1 then
                    printfn "[*] A single space will not be permitted"
                else
                    printfn "[*] Multiple spaces will not be permitted"

            else
                printfn "[*] '%s' is not considered to be a UoR ID" id

            // Continue iterating throughout the while loop.
            continueLooping <- true
    done
            
    // Return the id value.
    (id)

let getAge() =
    // Declare variables for usage.
    let mutable continueLooping = true
    let mutable age : int32 = 0
    // Construct a while loop for validation purposes.
    while continueLooping do

        // Ask the user to enter the student ID.
        Console.WriteLine("\nPlease enter the student age: ")
        let mutable str = Console.ReadLine()

        // Get rid of the trailing whitespaces.
        str <- str.Trim()

        if Int32.TryParse(str, &age) then
            // Yay! The age input is considered an integer, but...
            // Set conditions for abnormally large and impossibly small age ranges.
            if (age < 0) then
                // If the age is in the negatives, the input is invalid.
                printfn "[*] Age cannot range in the negatives"
                // Continue looping...
                continueLooping <- true
           
            // Validate for extremely young ages.
            else if (age <= 17) then
                // Notify the user that the earliest age for admission is 17.
                printfn "[*] The earliest age for admission to UoR is 18"
                // Continue looping...
                continueLooping <- true
            
            else if (age > 118) then
                // If the age is above 118, the input is invalid.
                // The oldest man on Earth, Kane Tanaka, is currently 118 years old.
                Console.WriteLine("[*] The oldest living person isn't even that old");
                continueLooping <- true

            else 
                // If input passes through all of those conditions, it is considered valid.
                // Hence, exit this loop.
                printfn "[+] '%d' has been accepted" age
                continueLooping <- false

        // Determine if the input was an empty entry.
        else if(String.IsNullOrEmpty str) then
            printfn "[*] Empty entries are not allowed"
            continueLooping <- true

        else
            // Notify the user if the input was completely comprised of letters.
            if(strContainsOnlyLetter str) then
                printfn "[*] Word(s) do not represent an age"
            // Determine if the forename input contains letters and numbers.
            if strContainsLettersAndNumbers(str) = true then
                printfn "[*] Ages are not composed of letters and numbers"
                continueLooping <- true
            // Determine if the dob input contains space(s).
            if containsSpace(str) <> 0 then
                if containsSpace(str) = 1 then
                    printfn "[*] A single space will not be permitted"
                else
                    printfn "[*] Multiple spaces will not be permitted"
            else
                // Let the user know that their input is not classified as an integer.
                printfn "[*] '%s' is not an integer" str
            // Continue iterating throughout the loop.
            continueLooping <- true
    done

    // Return the value of age.
    (age)

let getDOB(age : int32) =
    let mutable dob : string = ""
    let mutable tmp : string = ""
    let mutable continueLooping : bool = true
   
    let today = DateTime.Now

    let currentYear : int32 = today.ToString "yyyy" |> int32

    while continueLooping do
        Console.WriteLine("\nPlease enter student Date of Birth (dd/mm/yyyy): ")
        dob <- Console.ReadLine()
        // Get rid of the trailing whitespaces.
        dob <- dob.Trim()
        // Place the user input into the tmp variable.
        tmp <- dob
        
        if Regex.IsMatch(dob, "(^(((0[1-9]|1[0-9]|2[0-8])[\/](0[1-9]|1[012]))|((29|30|31)[\/](0[13578]|1[02]))|((29|30)[\/](0[4,6,9]|11)))[\/](19|[2-9][0-9])\d\d$)|(^29[\/]02[\/](19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)") then
     
            // Break up the validated date into a character array
            let dateArr : char[] = dob.ToCharArray()
            // Declare the day, month and year slot from the dateArr.
            let day : char[] = dateArr.[0..1]
            let month : char[] = dateArr.[3..4]
            let year : int32 = dob.[6..] |> int32

            // Determine if the year slot is more than the current year.
            if year > currentYear then
                printfn "[*] %d has not been in occurence yet" year
                continueLooping <- true

            // Switch the months and day positions to meet the requirement of...
            // DateTime format
            dateArr.[0..1] <- month
            dateArr.[3..4] <- day
            // Set the new dob value which will be mm/dd/yyyy
            dob <- System.String(dateArr)

            // Convert the string input into a DateTime variable
            let fromDate = DateTime.Parse(dob)
            // Calculate the number of days since the Student DOB
            let mutable totalDays = (today - fromDate).Days

            // Declare some counters
            let mutable dayCounter : int32 = 0
            let mutable numRange : int32 = 0

            // Iterate 'age' number of times.
            for i = 1 to age + 1 do
                // Determine if the loop is on the age + 1 iteration.
                if i = age + 1 then
                    // Determine if the year is a leap year.
                    if i % 4 = 0 then
                        // Since this is the last iteration, add dayCounter...
                        // value with 366.
                        numRange <- dayCounter + 366
                    else
                        // Add 365 to dayCounter (normal year)
                        numRange <- dayCounter + 365
                else
                    // Determine if i is considered a leap year.
                    if i % 4 = 0 then
                        // Add 366 to the dayCounter.
                        dayCounter <- dayCounter + 366

                    else
                        // Add 365 to the dayCounter.
                        dayCounter <- dayCounter + 365
            done

            // Declare the float variable versions of dayCounter and numRange.
            let dayCounterFloat : float = float dayCounter
            let numRangeFloat : float = float numRange

            // Determine the range the student was expected to be born.
            let endBetween = today.AddDays(-dayCounterFloat)
            let startBetween = today.AddDays(-numRangeFloat)

            // Convert startBetween and endBetween into string and store...
            // inside new variables of string type.
            let mutable formattedEnd : string = endBetween.ToString "MM/dd/yyyy"
            let mutable formattedStart : string = startBetween.ToString "MM/dd/yyyy"

            // Utilize the convertDateToWord function to receive the worded...
            // format of the string date.
            formattedEnd <- convertDateToWord(formattedEnd)
            formattedStart <- convertDateToWord(formattedStart)
            // Convert dob to a worded format.
            let formattedDOB : string = convertDateToWord(dob)

            // Determine if the age is within the proper range according...
            // to the age.
            if totalDays <= numRange && totalDays >= dayCounter then
                // Conversion was a success. Exit the loop.
                printfn "[+] '%s' has been accepted" formattedDOB
                // Set the dob value as the original (fixed) input value.
                dob <- tmp
                continueLooping <- false
            else
                // Inform the user that their input does not correlate to age.
                printfn "[*] Date of Birth does not correlate with the student age"
                printfn "[*] Expected birth of student: %s - %s" formattedStart formattedEnd
                // Continue to iterate throughout the while loop.
                continueLooping <- true
        else
            // Determine if the dob input contains space(s).
            if containsSpace(dob) <> 0 then
                if containsSpace(dob) = 1 then
                    printfn "[*] A single space will not be permitted"
                else
                    printfn "[*] Multiple spaces will not be permitted"

            // Determine if the user inputted three blocks of numbers...
            // separated by a backslash.
            if Regex.IsMatch(dob, "([0-9]{2}/[0-9]{2}/[0-9]{4})$") then
                // Declare each variable as a specific section of the input.
                let dayInput : int32 = dob.[0..1] |> int32
                let monthInput : int32 = dob.[3..4] |> int32
                let yearInput : int32 = dob.[6..9] |> int32

                // Find out if the user entered a number less than or equal...
                // to 0 or more than or equal to 32 in the day slot.
                if dayInput <= 0 || dayInput >= 32 then
                    printfn "[*] Days in a month do not exceed 31 or fall under 1"
                // Find out if the month slot is less than or equal to 0...
                // or more than 12.
                if monthInput <= 0 || monthInput > 12 then
                    printfn "[*] Months should not be under 1 or surpass 12"
                // Determine if the year slot is more than the current year.
                if yearInput > currentYear then
                    printfn "[*] %d has not even happened yet" yearInput
                else
                    // All else, display generic error message.
                    printfn "[*] '%s' is the incorrect format" dob
                    printfn "[*] The correct format: (dd/mm/yyyy)"

            // Determine if the input was an empty entry.
            else if(String.IsNullOrEmpty dob) then
                printfn "[*] Empty entries are not allowed"

            // Determine if the input is fully comprised of letters.
            else if(strContainsOnlyLetter(dob)) then
                // Set condition for input that is one letter long.
                if dob.Length = 1 then
                    printfn "[*] One letter is unacceptable for a date"
                else
                    printfn "[*] Dates are not fully comprised of letters"

                printfn "[*] '%s' will not be accepted" dob

            // Determine if the input is fully comprised of numbers.
            else if(strContainsOnlyNumber(dob)) then
                // Set condition for input that is one number.
                if dob.Length = 1 then
                    printfn "[*] One number cannot represent a date"
                else
                    printfn "[*] Dates are not fully comprised of numbers"
                    printfn "[*] Backslashes are involved... (dd/mm/yyyy)"

            // Determine if the dob input contains letters and numbers.
            else if strContainsLettersAndNumbers(dob) = true then
                printfn "[*] Dates are not composed of letters and numbers"

            // Find out if the user accidentally used dashes instead of backslashes.
            else if Regex.IsMatch(dob, "([0-9]{2}-[0-9]{2}-[0-9]{4})$") then
                printfn "[*] Format requires backslashes between the day, month and year"
                
            else
                // All else, display a generic error message.
                printfn "[*] '%s' is not considered to be a date in dd/mm/yyyy format" dob

            // Continue to iterate throughout the while loop.
            continueLooping <- true
    done

    (dob)

// Create a function that will auto generate an email for the student.
let getEmail(surname : string, forename : string) =
    // Set the email.
    // Declare variables.
    let mutable lastname = ""
    let mutable firstname = ""
    let mutable email : string = ""
    let mutable firsthalf : string = ""
    let firstHalfLength : int32 = 7
    let finalLength : int32 = 8

    // Declare dash, period and apostrophe variable 
    let dashChar : string = "-"
    let apostropheChar : string = "'"
    let periodChar : string = "."

    // Remove any special characters like "-", "'" and "." within both names
    lastname <- strip apostropheChar surname
    firstname <- strip apostropheChar forename

    lastname <- strip dashChar lastname
    firstname <- strip dashChar firstname

    lastname <- strip periodChar lastname
    firstname <- strip periodChar firstname

    // Determine if the character length of the surname exceeds 7
    if (String.length lastname > firstHalfLength) then
        // If the statement is true, remove the ending characters...
        // until the surname is at length 7
        firsthalf <- lastname.[0..6]
    else
        // Else... the full surname will suffice
        firsthalf <- lastname

    // Declare a string type variable that equates to the...
    // concatenation of the firsthalf and the forename
    let mutable result : string = firsthalf + firstname

    // Determine if the length of the result is less than 8
    if (String.length result < finalLength) then
        // The result will equate to the firsthalf and forename...
        // if the statement is true
        result <- firsthalf + firstname

    else
        // Otherwise, remove end characters of the concatenated...
        // result until the length eqauls to 8
        result <- result.[0..7]

    // Convert all characters within the result to lowercase
    result <- result.ToLower();
    // The final result show be displayed as the result...
    // concatenated with "@roehampton.ac.uk"
    result <- result + "@roehampton.ac.uk"
     
    email <- result
    (email)

let promptUser() =
    let mutable continueLooping = true
    let mutable continueLoopingAgain = true
   
    // Declare a list of Student type
    let mutable objectList : Student array = [||]

    while continueLooping do
        // Get all of the variable values by requesting the prompts from functions.
        let forename : string = getForename()
        let surname : string = getSurname()
        let age : int32 = getAge()
        let dob : string = getDOB(age)
        let id : string = getID(surname)
        let email : string = getEmail(surname, forename)

        // Create a Student object
        //let student = new Student(forename, surname, age, dob, id, email)
        let student : Student = new Student(forename, surname, age, dob, id, email)
        
        // Add the Student object into the Student array.
        objectList <- Array.append objectList [|student|]

        // Print the details of thr new student that has been added.
        printfn "\nThe following has been entered into the Student List:\n%A" student

        // Reset the continueLoopingAgain bool for 'Another Student?' Prompt.
        continueLoopingAgain <- true

        while continueLoopingAgain do
            // Ask the user if they wish to add more students.
            Console.WriteLine("\nWould you like to add another student? (Yes or No): ")
            let mutable response = Console.ReadLine()

            // Eliminate all of the trailing whitespaces within input.
            response <- response.Trim()
            // Convert the input to all lowercase letters.
            response <- response.ToLower()

            // Determine if 'yes' was entered.
            if response = "yes" then
                // Reprompt the user for new student details.
                continueLooping <- true
                continueLoopingAgain <- false
            // Determine if 'no' was entered.
            else if response = "no" then
                // Exit both while loops.
                continueLooping <- false
                continueLoopingAgain <- false
            // Determine if the input was an empty entry.
            else if(String.IsNullOrEmpty dob) then
                printfn "[*] Empty entries are not allowed"
                continueLoopingAgain <- true
            else
                printfn "[*] Only 'Yes' or 'No' responses will be accepted"
                // Reprompt the user with the "Another Student?" Prompt.
                continueLoopingAgain <- true
        done
    done

    // Return the value within the studentList
    (objectList) 
    
let findStudent(result : Student array) =
    let mutable continueLooping : bool = true

    while continueLooping do
        Console.WriteLine("\n\nFind a Student (Enter Student name, surname or ID ['q' to exit]): ")
        let mutable target : string = Console.ReadLine()

        // Remove all of the trailing whitespaces within input
        target <- target.Trim()
        // Declare a storage variable to store the original (fixed) input
        let mutable tmp = target

        // Determine the amount of words make up the input
        let number = count(target)

        if(number = 1) then
            let mutable updatedTarget =
                target
                |> capitalizeFirstLetter

            updatedTarget <- capitalizeAfterOccurence(updatedTarget)

            if Regex.IsMatch(updatedTarget, "^[A-Z]([a-z]{1,32}((['.-]{1}([A-Z]{1}))?))+$") then
                target <- updatedTarget

            else
                updatedTarget <- target.[0..2]
                updatedTarget <- updatedTarget.ToUpper()
                updatedTarget <- updatedTarget + target.[3..]

                if Regex.IsMatch(updatedTarget, "^[A-Z]{3}([0-9]{8})$") then
                    target <- updatedTarget

                else
                    target <- tmp
        else
            let mutable fullString = ""
            // Split the input into separate items within an array.
            let wordsArray = target.Split([|" "|], StringSplitOptions.None)
            let mutable capWordsList : string list = []

            // Iterate throughout the wordsArray
            for word in wordsArray do
                // Capitalize each item within the array.
                let mutable updatedWord : string =
                    word
                    |> capitalizeFirstLetter
                // Place the capitalized word in a new list.
                capWordsList <- updatedWord :: capWordsList

            // Reverse the order of the list.
            capWordsList <- reverseList capWordsList
            // Build the string using concatenation.
            fullString <- String.concat " " capWordsList
            // Set the target as the new string.
            target <- fullString

        let mutable counter : int32 = 0

        let mutable multipleStudentsList : string list = []
        let mutable singleStudentList : string list = []

        for element in result do

            let forename : string = element.Forename
            let surname : string = element.Surname
            let age : int32 = element.Age
            let dob : string = element.DOB
            let id : string = element.ID
            let email : string = element.Email
            let fullName : string = String.concat " " [forename; surname]

            let singleStudentTuple = String.Format("({0}, {1}, {2}, {3}, {4}, {5})", forename, surname, age, dob, id, email)
            let multipleStudentsTuple = String.Format("({0}, {1}, {2}, {3}, {4})", fullName, age, dob, id, email)

            if forename = target || surname = target || id = target || fullName = target then
                counter <- counter + 1

                if counter = 1 then
                    // Ensure that this message is only displayed once.
                    printfn "\nHi, I have found the Student:"

                // Append the tuples to the corresponding lists.
                multipleStudentsList <- multipleStudentsTuple :: multipleStudentsList
                singleStudentList <- singleStudentTuple :: singleStudentList
        done

        if counter = 1 then
            for element in singleStudentList do
                printfn "\n-> %s" element
        else if counter > 1 then
            for element in multipleStudentsList do
                printfn "\n-> %s" element
        else
            // Determine if 'q' was entered...
            if target = "q" then
                // Continue the loop.
                continueLooping <- false
            else if target = "" then
                printfn "[*] Empty entries will not be permitted"
            else
                printfn "\n[*] There is no Student of the name, surname or ID: '%s'" target

    done

let main() =

    // Get the studentList from the promptUser() function
    let result : Student array = promptUser()

    // Print all of the Students with their values.
    printfn "\n--------------------Students within the database--------------------"
    for student in result do
        printfn "\n%A" student

    // Execute the findStudent() function which enables the user to find...
    // a specific student.
    findStudent(result)

    // Exit the program.
    printfn "\nExiting the Program..."
    
// Call upon the main function for execution
main()