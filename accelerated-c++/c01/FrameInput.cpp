//
//  FrameInput.cpp
//  Cpp.TDD
//
//  Created by Yohan Lee on 8/23/19.
//  Copyright © 2019 Yohan Lee. All rights reserved.
//

#include <iostream>
#include <string>

int FrameInput()
{
    std:: cout << "Please enter your first name: ";
    std::string name; //define name
    std::cin >> name; //read into name
    
    //build the message that we intend to write
    const std::string greeting = "Greetings, " + name + "!";
    
    //build the second and fourth lines of the output
    const std::string spaces(greeting.size(), ' ');
    const std::string second = "* " + spaces + " *";
    
    //build the first and fifth lines of the output
    const std::string first(second.size(), '*');
    
    //write it all
    std::cout << std::endl;
    std::cout << first << std::endl;
    std::cout << second << std::endl;
    std::cout << "* " << greeting << " *" << std::endl;
    std::cout << second << std::endl;
    std::cout << first << std::endl;
    return 0;
}

int main(int argc, const char * argv[]) {
    return FrameInput();
}
