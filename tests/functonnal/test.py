import subprocess
import os

example_programs = {
    "tests/functonnal/testFiles/return/return.mll": 1,
    "tests/functonnal/testFiles/return/retour.mll": 1,
    "tests/functonnal/testFiles/return/retourne.mll": 1,
    "tests/functonnal/testFiles/Var/Int.mll": 4,
    "tests/functonnal/testFiles/Var/Char.mll": 70,
    "tests/functonnal/testFiles/prog/fibRec.mll": 55,
}

def get_haskell_executable():
    try:
        result = subprocess.run(["stack", "path", "--local-install-root"], capture_output=True, text=True)
        if result.returncode != 0:
            raise Exception(f"Failed to get stack path: {result.stderr}")
        
        install_root = result.stdout.strip()
        executable_path = os.path.join(install_root, "bin", "glados")
        return executable_path
    except Exception as e:
        print(f"An error occurred while getting the Haskell executable path: {e}")
        exit(1)

def print_green(text):
    print(f"\033[92m{text}\033[0m")

def print_red(text):
    print(f"\033[91m{text}\033[0m")

def run_test(program, expected_return_value, haskell_executable):
    try:
        result = subprocess.run([haskell_executable, "-e", program], capture_output=True, text=True)
        
        print(f"Running {program}:")
        print("Output:")
        print(result.stdout)
        print("Error:")
        print(result.stderr)
        print("Return Code:", result.returncode)
        print("-" * 40)
        
        if result.returncode == expected_return_value:
            print_green(f"Test {program} passed.\n")
            return True
        else:
            print_red(f"Test {program} failed. Expected {expected_return_value}, but got {result.returncode}.\n")
            return False
    except Exception as e:
        print_red(f"An error occurred while running {program}: {e}\n")
        return False

def main():
    haskell_executable = get_haskell_executable()
    passed_tests = 0
    failed_tests = 0

    for program, expected_return_value in example_programs.items():
        if os.path.exists(program):
            if run_test(program, expected_return_value, haskell_executable):
                passed_tests += 1
            else:
                failed_tests += 1
        else:
            print_red(f"Example program {program} does not exist.")
            failed_tests += 1

    print("\nTest Summary:")
    print_green(f"Passed tests: {passed_tests}")
    print_red(f"Failed tests: {failed_tests}\n")

if __name__ == "__main__":
    main()