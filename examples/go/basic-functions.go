// Basic Go Functions - Examples for ABAP Translation
// These examples demonstrate various Go constructs that can be translated to ABAP

package main

import (
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
)

// Simple function with single return value
func greetUser(name string) string {
	return "Hello, " + name + "!"
}

// Function with multiple parameters and return values
func calculateAreaPerimeter(length, width float64) (float64, float64) {
	area := length * width
	perimeter := 2 * (length + width)
	return area, perimeter
}

// Function with named return values
func divideNumbers(a, b float64) (result float64, err error) {
	if b == 0 {
		err = errors.New("division by zero")
		return
	}
	result = a / b
	return
}

// Function with conditional logic
func checkAge(age int) string {
	if age >= 18 {
		return "Adult"
	} else {
		return "Minor"
	}
}

// Function with loops
func sumNumbers(numbers []int) int {
	total := 0
	for _, num := range numbers {
		total += num
	}
	return total
}

// Function with slice manipulation
func filterEvenNumbers(numbers []int) []int {
	var evenNumbers []int
	for _, num := range numbers {
		if num%2 == 0 {
			evenNumbers = append(evenNumbers, num)
		}
	}
	return evenNumbers
}

// Function with map usage
func countWords(text string) map[string]int {
	words := strings.Fields(strings.ToLower(text))
	wordCount := make(map[string]int)
	
	for _, word := range words {
		wordCount[word]++
	}
	
	return wordCount
}

// Function with struct
type User struct {
	Name     string
	Email    string
	Age      int
	IsActive bool
}

func createUser(name, email string, age int) User {
	return User{
		Name:     name,
		Email:    email,
		Age:      age,
		IsActive: true,
	}
}

// Method on struct
func (u User) GetFullInfo() string {
	status := "inactive"
	if u.IsActive {
		status = "active"
	}
	return fmt.Sprintf("Name: %s, Email: %s, Age: %d, Status: %s", 
		u.Name, u.Email, u.Age, status)
}

// Function with pointer
func updateUserAge(user *User, newAge int) {
	user.Age = newAge
}

// Function with variadic parameters
func calculateAverage(numbers ...float64) float64 {
	if len(numbers) == 0 {
		return 0
	}
	
	var sum float64
	for _, num := range numbers {
		sum += num
	}
	
	return sum / float64(len(numbers))
}

// Function with defer
func processFile(filename string) error {
	fmt.Printf("Opening file: %s\n", filename)
	
	// Simulate file operations
	defer func() {
		fmt.Printf("Closing file: %s\n", filename)
	}()
	
	if filename == "" {
		return errors.New("filename cannot be empty")
	}
	
	fmt.Printf("Processing file: %s\n", filename)
	return nil
}

// Recursive function
func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

// Function with switch statement
func getGrade(score int) string {
	switch {
	case score >= 90:
		return "A"
	case score >= 80:
		return "B"
	case score >= 70:
		return "C"
	case score >= 60:
		return "D"
	default:
		return "F"
	}
}

// Function with type assertion
func processInterface(data interface{}) string {
	switch v := data.(type) {
	case string:
		return "String: " + v
	case int:
		return "Integer: " + strconv.Itoa(v)
	case float64:
		return "Float: " + strconv.FormatFloat(v, 'f', 2, 64)
	case bool:
		return "Boolean: " + strconv.FormatBool(v)
	default:
		return "Unknown type"
	}
}

// Function with channel (simplified example)
func generateNumbers(count int) <-chan int {
	ch := make(chan int)
	
	go func() {
		defer close(ch)
		for i := 1; i <= count; i++ {
			ch <- i
		}
	}()
	
	return ch
}

// Function with error handling pattern
func validateEmail(email string) error {
	if email == "" {
		return errors.New("email cannot be empty")
	}
	
	if !strings.Contains(email, "@") {
		return errors.New("email must contain @")
	}
	
	parts := strings.Split(email, "@")
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return errors.New("invalid email format")
	}
	
	if !strings.Contains(parts[1], ".") {
		return errors.New("domain must contain a dot")
	}
	
	return nil
}

// Function with complex data structure
type Stats struct {
	Min, Max, Sum float64
	Count         int
	Average       float64
}

func calculateStats(numbers []float64) Stats {
	if len(numbers) == 0 {
		return Stats{}
	}
	
	stats := Stats{
		Min:   numbers[0],
		Max:   numbers[0],
		Count: len(numbers),
	}
	
	for _, num := range numbers {
		stats.Sum += num
		if num < stats.Min {
			stats.Min = num
		}
		if num > stats.Max {
			stats.Max = num
		}
	}
	
	stats.Average = stats.Sum / float64(stats.Count)
	return stats
}

func main() {
	// Test basic functions
	fmt.Println("=== Basic Function Tests ===")
	
	userName := "Alice"
	greeting := greetUser(userName)
	fmt.Println(greeting)
	
	area, perimeter := calculateAreaPerimeter(10, 5)
	fmt.Printf("Area: %.2f, Perimeter: %.2f\n", area, perimeter)
	
	result, err := divideNumbers(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("Division result: %.2f\n", result)
	}
	
	ageGroup := checkAge(25)
	fmt.Printf("Age group: %s\n", ageGroup)
	
	numbers := []int{1, 2, 3, 4, 5}
	sum := sumNumbers(numbers)
	fmt.Printf("Sum: %d\n", sum)
	
	evenNums := filterEvenNumbers([]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
	fmt.Printf("Even numbers: %v\n", evenNums)
	
	wordCount := countWords("hello world hello go")
	fmt.Printf("Word count: %v\n", wordCount)
	
	// Test struct and methods
	fmt.Println("\n=== Struct and Method Tests ===")
	
	user := createUser("John Doe", "john@example.com", 30)
	fmt.Println(user.GetFullInfo())
	
	updateUserAge(&user, 31)
	fmt.Printf("Updated age: %d\n", user.Age)
	
	avg := calculateAverage(1.5, 2.5, 3.5, 4.5, 5.5)
	fmt.Printf("Average: %.2f\n", avg)
	
	// Test file processing with defer
	fmt.Println("\n=== File Processing Test ===")
	err = processFile("test.txt")
	if err != nil {
		fmt.Printf("File processing error: %v\n", err)
	}
	
	// Test recursive function
	fmt.Println("\n=== Recursive Function Test ===")
	fact := factorial(5)
	fmt.Printf("Factorial of 5: %d\n", fact)
	
	// Test switch statement
	fmt.Println("\n=== Switch Statement Test ===")
	scores := []int{95, 87, 72, 64, 58}
	for _, score := range scores {
		grade := getGrade(score)
		fmt.Printf("Score %d: Grade %s\n", score, grade)
	}
	
	// Test interface processing
	fmt.Println("\n=== Interface Processing Test ===")
	testData := []interface{}{"hello", 42, 3.14, true, []int{1, 2, 3}}
	for _, data := range testData {
		result := processInterface(data)
		fmt.Println(result)
	}
	
	// Test email validation
	fmt.Println("\n=== Email Validation Test ===")
	emails := []string{
		"test@example.com",
		"invalid-email",
		"",
		"test@",
		"@example.com",
		"test@example",
	}
	
	for _, email := range emails {
		err := validateEmail(email)
		if err != nil {
			fmt.Printf("Email '%s': Invalid - %v\n", email, err)
		} else {
			fmt.Printf("Email '%s': Valid\n", email)
		}
	}
	
	// Test statistics calculation
	fmt.Println("\n=== Statistics Test ===")
	testNumbers := []float64{1.5, 2.8, 3.2, 4.7, 5.1, 6.9, 7.3}
	stats := calculateStats(testNumbers)
	fmt.Printf("Stats - Min: %.2f, Max: %.2f, Sum: %.2f, Count: %d, Avg: %.2f\n",
		stats.Min, stats.Max, stats.Sum, stats.Count, stats.Average)
	
	// Test channel (simplified)
	fmt.Println("\n=== Channel Test ===")
	fmt.Print("Generated numbers: ")
	for num := range generateNumbers(5) {
		fmt.Printf("%d ", num)
	}
	fmt.Println()
}