/**
 * JavaScript Control Structures - Examples for ABAP Translation
 * Demonstrates various control flow constructs
 */

// If-else statements
function processScore(score) {
  if (score >= 90) {
    return 'A';
  } else if (score >= 80) {
    return 'B';
  } else if (score >= 70) {
    return 'C';
  } else if (score >= 60) {
    return 'D';
  } else {
    return 'F';
  }
}

// Switch statement
function getDayName(dayNumber) {
  switch (dayNumber) {
    case 1:
      return 'Monday';
    case 2:
      return 'Tuesday';
    case 3:
      return 'Wednesday';
    case 4:
      return 'Thursday';
    case 5:
      return 'Friday';
    case 6:
      return 'Saturday';
    case 7:
      return 'Sunday';
    default:
      return 'Invalid day';
  }
}

// While loop
function countDown(start) {
  const results = [];
  let current = start;
  
  while (current > 0) {
    results.push(current);
    current--;
  }
  
  return results;
}

// Do-while loop
function generateRandomNumbers(count) {
  const numbers = [];
  let i = 0;
  
  do {
    numbers.push(Math.floor(Math.random() * 100));
    i++;
  } while (i < count);
  
  return numbers;
}

// For loop with break and continue
function findFirstEven(numbers) {
  for (let i = 0; i < numbers.length; i++) {
    const num = numbers[i];
    
    if (num < 0) {
      continue; // Skip negative numbers
    }
    
    if (num % 2 === 0) {
      return num; // Return first even number
    }
  }
  
  return null; // No even number found
}

// Nested loops
function createMultiplicationTable(size) {
  const table = [];
  
  for (let i = 1; i <= size; i++) {
    const row = [];
    for (let j = 1; j <= size; j++) {
      row.push(i * j);
    }
    table.push(row);
  }
  
  return table;
}

// For-of loop
function processItems(items) {
  const processed = [];
  
  for (const item of items) {
    if (item && typeof item === 'string') {
      processed.push(item.toUpperCase());
    }
  }
  
  return processed;
}

// Try-catch-finally
function processData(data) {
  let result = null;
  
  try {
    if (!data || data.length === 0) {
      throw new Error('Data is empty');
    }
    
    result = data.map(item => item * 2);
    
  } catch (error) {
    console.error('Processing error:', error.message);
    result = [];
    
  } finally {
    console.log('Processing completed');
  }
  
  return result;
}

// Ternary operator
function getStatusMessage(isActive) {
  return isActive ? 'User is active' : 'User is inactive';
}

// Complex nested control structures
function analyzeNumbers(numbers) {
  const analysis = {
    positive: 0,
    negative: 0,
    zero: 0,
    even: 0,
    odd: 0
  };
  
  for (const num of numbers) {
    // Check sign
    if (num > 0) {
      analysis.positive++;
    } else if (num < 0) {
      analysis.negative++;
    } else {
      analysis.zero++;
    }
    
    // Check even/odd
    if (num !== 0) {
      if (num % 2 === 0) {
        analysis.even++;
      } else {
        analysis.odd++;
      }
    }
  }
  
  return analysis;
}

// Example usage
const scores = [95, 87, 72, 64, 58];
scores.forEach(score => {
  console.log(`Score ${score}: Grade ${processScore(score)}`);
});

console.log('Day 3:', getDayName(3));
console.log('Day 8:', getDayName(8));

const countdown = countDown(5);
console.log('Countdown:', countdown);

const randomNums = generateRandomNumbers(5);
console.log('Random numbers:', randomNums);

const testNumbers = [-1, 3, 5, 8, 11];
const firstEven = findFirstEven(testNumbers);
console.log('First even number:', firstEven);

const multiTable = createMultiplicationTable(3);
console.log('Multiplication table:', multiTable);

const items = ['hello', 'world', null, 'test'];
const processed = processItems(items);
console.log('Processed items:', processed);

const testData = [1, 2, 3, 4];
const processedData = processData(testData);
console.log('Processed data:', processedData);

console.log('Status:', getStatusMessage(true));

const numberAnalysis = analyzeNumbers([1, -2, 0, 4, -5, 6]);
console.log('Number analysis:', numberAnalysis);