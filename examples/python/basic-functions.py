"""
Basic Python Functions - Examples for ABAP Translation
These examples demonstrate various Python constructs that can be translated to ABAP
"""

def greet_user(name):
    """Simple function with string concatenation"""
    return f"Hello, {name}!"

def calculate_area(length, width):
    """Function with multiple parameters and calculations"""
    area = length * width
    return area

def check_age(age):
    """Function with conditional logic"""
    if age >= 18:
        return "Adult"
    else:
        return "Minor"

def sum_numbers(numbers):
    """Function with loops"""
    total = 0
    for num in numbers:
        total += num
    return total

def create_user(name, email, age):
    """Function with dictionary manipulation"""
    user = {
        'name': name,
        'email': email,
        'age': age,
        'is_active': True
    }
    return user

def filter_even_numbers(numbers):
    """Function with list comprehension and filtering"""
    return [num for num in numbers if num % 2 == 0]

def format_name(first_name, last_name):
    """Function with string operations"""
    return f"{first_name.upper()}, {last_name.upper()}"

def divide_numbers(a, b):
    """Function with error handling"""
    try:
        if b == 0:
            raise ValueError("Division by zero")
        return a / b
    except ValueError as e:
        print(f"Error: {e}")
        return None

def fibonacci(n):
    """Recursive function"""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def process_text(text, operation):
    """Function with lambda and higher-order functions"""
    operations = {
        'upper': lambda x: x.upper(),
        'lower': lambda x: x.lower(),
        'reverse': lambda x: x[::-1],
        'length': lambda x: len(x)
    }
    
    if operation in operations:
        return operations[operation](text)
    else:
        return text

def analyze_list(data):
    """Function with multiple return values"""
    if not data:
        return 0, 0, 0
    
    total = sum(data)
    average = total / len(data)
    maximum = max(data)
    
    return total, average, maximum

def process_kwargs(**kwargs):
    """Function with keyword arguments"""
    result = {}
    for key, value in kwargs.items():
        if isinstance(value, str):
            result[key] = value.title()
        elif isinstance(value, (int, float)):
            result[key] = value * 2
        else:
            result[key] = value
    return result

def validate_email(email):
    """Function with complex validation"""
    if not email:
        return False, "Email is empty"
    
    if '@' not in email:
        return False, "Email must contain @"
    
    parts = email.split('@')
    if len(parts) != 2:
        return False, "Email format is invalid"
    
    username, domain = parts
    if not username or not domain:
        return False, "Username or domain is empty"
    
    if '.' not in domain:
        return False, "Domain must contain a dot"
    
    return True, "Email is valid"

def batch_process(items, batch_size=3):
    """Generator function"""
    for i in range(0, len(items), batch_size):
        yield items[i:i + batch_size]

# Class example
class Calculator:
    """Simple calculator class"""
    
    def __init__(self):
        self.history = []
    
    def add(self, a, b):
        result = a + b
        self.history.append(f"{a} + {b} = {result}")
        return result
    
    def subtract(self, a, b):
        result = a - b
        self.history.append(f"{a} - {b} = {result}")
        return result
    
    def multiply(self, a, b):
        result = a * b
        self.history.append(f"{a} * {b} = {result}")
        return result
    
    def divide(self, a, b):
        if b == 0:
            raise ValueError("Cannot divide by zero")
        result = a / b
        self.history.append(f"{a} / {b} = {result}")
        return result
    
    def get_history(self):
        return self.history.copy()
    
    def clear_history(self):
        self.history.clear()

# Main execution
if __name__ == "__main__":
    # Test basic functions
    user_name = "Alice"
    greeting = greet_user(user_name)
    print(greeting)
    
    area = calculate_area(10, 5)
    print(f"Area: {area}")
    
    age_group = check_age(25)
    print(f"Age group: {age_group}")
    
    numbers = [1, 2, 3, 4, 5]
    total = sum_numbers(numbers)
    print(f"Sum: {total}")
    
    user = create_user("John Doe", "john@example.com", 30)
    print(f"User: {user}")
    
    even_nums = filter_even_numbers([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    print(f"Even numbers: {even_nums}")
    
    formatted_name = format_name("John", "Doe")
    print(f"Formatted name: {formatted_name}")
    
    result = divide_numbers(10, 2)
    print(f"Division result: {result}")
    
    fib_result = fibonacci(8)
    print(f"Fibonacci(8): {fib_result}")
    
    processed_text = process_text("Hello World", "upper")
    print(f"Processed text: {processed_text}")
    
    data = [1, 2, 3, 4, 5]
    total, avg, maximum = analyze_list(data)
    print(f"Analysis - Total: {total}, Average: {avg}, Max: {maximum}")
    
    kwargs_result = process_kwargs(name="john", age=30, city="New York")
    print(f"Kwargs result: {kwargs_result}")
    
    is_valid, message = validate_email("test@example.com")
    print(f"Email validation: {is_valid}, {message}")
    
    # Test batch processing
    items = list(range(1, 11))
    print("Batch processing:")
    for batch in batch_process(items, 3):
        print(f"Batch: {batch}")
    
    # Test calculator class
    calc = Calculator()
    print(f"Addition: {calc.add(5, 3)}")
    print(f"Subtraction: {calc.subtract(10, 4)}")
    print(f"Multiplication: {calc.multiply(6, 7)}")
    print(f"Division: {calc.divide(15, 3)}")
    
    print("Calculator history:")
    for entry in calc.get_history():
        print(f"  {entry}")