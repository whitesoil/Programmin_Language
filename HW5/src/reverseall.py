def parser(ls):       
    temp_list = []
    i = 0
    while i < len(ls) :        
        if ls[i] != ',' and ls[i] != '[' and ls[i] != ']' and ls[i] != ' ':
            temp_list.insert(0,ls[i])
            i += 1
        elif ls[i] == '[':     
            recur = parser(ls[i+1:])       
            temp_list.insert(0,recur)                            
            temp = ls[i:]
            end = 0
            count = 0
            for j in range(0,len(temp)):
                if temp[j] == '[':
                    count += 1
                elif temp[j] == ']':
                    break           
            for j in range(0,len(temp)):
                if count == 0:
                    break
                elif temp[j] == ']':
                    count -= 1
                end += 1            
            i += end       
        elif ls[i] == ']':
            return temp_list
        else :
            i += 1            
    return temp_list
    
origin_input = input() 
origin_input = origin_input[origin_input.find('[')+1:len(origin_input)-2]
result = parser(origin_input)
print(result)
