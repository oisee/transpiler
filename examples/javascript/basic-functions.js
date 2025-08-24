/**
 * Basic JavaScript Functions - Examples for ABAP Translation
 * These examples demonstrate various JavaScript constructs that can be translated to ABAP
 */

// Simple function declaration
function greetUser(name) {
  return 'Hello, ' + name + '!';
}

// Function with multiple parameters and calculations
function calculateArea(length, width) {
  const area = length * width;
  return area;
}

// Function with conditional logic
function checkAge(age) {
  if (age >= 18) {
    return 'Adult';
  } else {
    return 'Minor';
  }
}

// Function with loops
function sumNumbers(numbers) {
  let total = 0;
  for (let i = 0; i < numbers.length; i++) {
    total += numbers[i];
  }
  return total;
}

// Function with object manipulation
function createUser(name, email, age) {
  const user = {
    name: name,
    email: email,
    age: age,
    isActive: true
  };
  return user;
}

// Function with array operations
function filterEvenNumbers(numbers) {
  const evenNumbers = [];
  for (const num of numbers) {
    if (num % 2 === 0) {
      evenNumbers.push(num);
    }
  }
  return evenNumbers;
}

// Function with string operations
function formatName(firstName, lastName) {
  return firstName.toUpperCase() + ', ' + lastName.toUpperCase();
}

// Function with error handling
function divideNumbers(a, b) {
  try {
    if (b === 0) {
      throw new Error('Division by zero');
    }
    return a / b;
  } catch (error) {
    console.log('Error:', error.message);
    return null;
  }
}

// Main execution
const userName = 'Alice';
const greeting = greetUser(userName);
console.log(greeting);

const area = calculateArea(10, 5);
console.log('Area:', area);

const ageGroup = checkAge(25);
console.log('Age group:', ageGroup);

const numbers = [1, 2, 3, 4, 5];
const sum = sumNumbers(numbers);
console.log('Sum:', sum);

const user = createUser('John Doe', 'john@example.com', 30);
console.log('User:', user);

const evenNums = filterEvenNumbers([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
console.log('Even numbers:', evenNums);

const formattedName = formatName('John', 'Doe');
console.log('Formatted name:', formattedName);

const result = divideNumbers(10, 2);
console.log('Division result:', result);