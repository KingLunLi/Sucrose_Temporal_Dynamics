// activates or assigns TTL to pin 8 
const int TTL4=8;
int value=0;  

void setup() 
   { 
      // Initializes serial communication at a baud rate of 9600.
      Serial.begin(9600); 
      pinMode(TTL4, OUTPUT);
      digitalWrite (TTL4, LOW);
      Serial.println("Connection established...");
   }
 
void loop() 
   {
     while (Serial.available())
        {
           value = Serial.read();
        }
     
     // If the received value is 'O', it sets pin 8 (TTL4) to HIGH (triggers laser).
     if (value == 'O')
        digitalWrite (TTL4, HIGH);
     
     // If the received value is 'X', it sets pin 8 (TTL4) to LOW (does not trigger laser).
     else if (value == 'X')
        digitalWrite (TTL4, LOW);
   } 
