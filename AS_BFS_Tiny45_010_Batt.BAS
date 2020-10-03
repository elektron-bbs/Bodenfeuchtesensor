'*******************************************************************************
'*   Description   : Funk-Bodenfeuchtesensor mit                               *
'*   Capacitive Soil Moisture Sensor v1.2 und ASK Transmitter SYN115           *
'*   Protokoll     : ArduinoSensor (https://github.com/RFD-FHEM/ArduinoSensor) *
'*   Revision      : 1.0                                                       *
'*   Controller    : ATTiny45                                                  *
'*   Stromaufnahme : ca. 15 mikroA @ 3,6 V (Saft LS 14500)                     *
'*   Compiler      : BASCOM-AVR  2.0.8.3                                       *
'*   Author        : UB , 2020                                                 *
'*   Web           : HTTP://WWW.Elektron-BBS.de                                *
'*******************************************************************************
$regfile = "attiny45.dat"
$crystal = 1000000
$hwstack = 32
$swstack = 16
$framesize = 16
$noramclear                                                                     ' Variablen nicht zurueck setzen
Stop Ac                                                                         ' Strom sparen
Stop Adc                                                                        ' Strom sparen

Wdtcr = &B00101001                                                              ' Watchdog Config 8 Sekunden und Watchdog On
Enable Interrupts                                                               ' Interrupts einschalten

'************************* K o n s t a n t e n *********************************
Const Ident = 1                                                                 ' Sensor-Ident, 0-63
Const Type = 10                                                                 ' Sensortyp, 0-31, 10 = raw
Const Tx_bit_anzahl = 40                                                        ' Anzahl zu sendende Bits
Const Tx_byte_anzahl = 5                                                        ' Anzahl zu sendende Bytes
Const Tx_repeats = 3                                                            ' Anzahl Wiederholungen
Const Tx_interval = 22                                                          ' Sendeintervall = Tx_interval * 8 Sekunden + 64 mS bis 8 Sekunden
Const Wdto_4s = &B00101000                                                      ' Watchdog Config 4 Sekunden und Watchdog On
Const Wdto_8s = &B00101001                                                      ' Watchdog Config 8 Sekunden und Watchdog On

'*************************** ungenutze Ports ***********************************
Config Portb.0 = Input : Portb.0 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portb.2 = Input : Portb.2 = 1                                            ' Port als Eingang, Pullup eingeschaltet

'************************** H a u p t p r o g r a m m **************************
Dim Newstart As Byte
Dim Wd_count As Byte                                                            ' Zaehler fuer Watchdog-Starts

Newstart = Mcusr And &B0000_0111                                                ' MCU Status Register (BORF,EXTRF,PORF)
If Newstart >= 1 Then
   Wd_count = 100                                                               ' Watchdog-Zaehler
   Mcusr = 0                                                                    ' Reset MCU Status Register
End If

Incr Wd_count

If Wd_count >= Tx_interval Then                                                 ' Tx_interval * 8 Sekunden
   ' Ports for connection
   Config Portb.1 = Output : Tx433 Alias Portb.1                                ' Data Sendemodul 433 MHz
   Config Portb.4 = Output : Ubsens Alias Portb.4                               ' Betriebsspannung Sensor
   ' Variablen TX 433 MHz
   Dim Tx_dbl As Double                                                         ' max. 64 Bit Sendepuffer
   Dim Tx_bit_nr As Byte                                                        ' Nummer zu sendendes Bit
   Dim Tx_byte As Byte                                                          ' zu sendendes Byte
   Dim Tx_byte_nr As Byte                                                       ' Nummer zu sendendes Byte
   Dim Value As Word
   Dim Trigger As Byte                                                          ' 0 = auto, 1 = manual
   Dim Battery As Byte                                                          ' 0-3, 0 = low, 2 = ok
   Dim Crc1 As Byte
   Dim Crc2 As Byte
   Dim Crc3 As Byte
   Dim X As Byte , Y As Byte                                                    ' temporaer
   Dim Raw As Word                                                              ' temporaer
   Dim Ar(5) As Byte                                                            ' temporaer

   Set Ubsens                                                                   ' Betriebsspannung Sensor einschalten

   If Wd_count >= 100 Then                                                      ' Neustart
      Wdtcr = Wdto_4s                                                           ' Watchdog Config 4 Sekunden und Watchdog On
      Waitms 200                                                                ' Bodenfeuchtesensor baucht ca. 200 mS bis Werte stabil sind.
      Trigger = 1                                                               ' manual
      Gosub Mess                                                                ' erst messen ...
      Gosub Send                                                                ' ... dann senden
      If Wd_count >= 115 Then                                                   ' 15 * 4 Sekunden = 60
         Wdtcr = Wdto_8s                                                        ' Watchdog Config 8 Sekunden und Watchdog On
         Wd_count = 0                                                           ' Watchdog-Zaehler zurueck setzen
      End If
   Else
      Wdtcr = Wdto_8s                                                           ' Watchdog Config 8 Sekunden und Watchdog On
      Trigger = 0                                                               ' auto
      ' Bodenfeuchtesensor baucht ca. 200 mS bis Werte stabil sind. Deshalb erst senden und dann messen
      Gosub Send                                                                ' erst senden ...
      Gosub Mess                                                                ' ... dann messen
      If Wd_count >= Tx_interval Then
         X = Ident And &B00000111
         X = X + 2                                                              ' min 64 mS, max 8,0 S
         X.5 = X.3                                                              ' WDP3
         X.3 = 1                                                                ' Bit 3 - WDE: Watchdog Enable
         Wdtcr = X                                                              ' Watchdog Config Sekunden je nach Ident und Watchdog On
         Wd_count = 0                                                           ' Watchdog-Zaehler zurueck setzen
      End If
   End If
End If

Config Powermode = Powerdown

end

'************************** U N T E R P R O G R A M M E ************************
Send:
   ' Werte uebernehmen und senden
   Shift Battery , Left , 6
   Shift Trigger , Left , 7
   Ar(1) = Trigger + Type                                                       ' Bit 8 = trigger, Sensor type 0-31
   Ar(2) = Battery + Ident                                                      ' Bit 0-5 = 6 bit device ID, bit 7-8 = 2 bit battery
   Ar(3) = Low(value)                                                           ' Data low byte
   Ar(4) = High(value)                                                          ' Data high byte
   Crc1 = 0
   For X = 1 To 4
      Crc2 = Ar(x)
      For Y = 0 To 7
         Crc3 = Crc1 Xor Crc2
         Crc3 = Crc3 And &H80
         Shift Crc1 , Left , 1
         If crc3 >= 1 Then
            Crc1 = Crc1 Xor 0x31
         End If
         Shift Crc2 , Left , 1
      Next y
   Next X
   Ar(5) = Crc1                                                                 ' CRC8
   Gosub Tx_433_send                                                            ' Daten senden
Return

Mess:
   ' ADC 12 Batteriespannung messen
   Config Adc = Single , Prescaler = Auto , Reference = Avcc
   Raw = Getadc(12)                                                             ' Dummy Messung auf MUX Kanal 12, der internen Bandgap Referenzspannung (1.1V)
   Waitms 1                                                                     ' mindestens 1 mS warten zwischen Einschalten ADC und GETADC
   Raw = Getadc(12)                                                             ' Messung auf MUX Kanal 12, der internen Bandgap Referenzspannung (1.1V)
   Stop Adc
   ' Battery 0-3, 0=low, 3=full
   Select Case Raw
      Case 0 To 314
         Battery = 3
      Case 315 To 323
         Battery = 2
      Case 324 To 333
         Battery = 1
      Case Else
         Battery = 0
   End Select
   ' ADC 3 Bodenfeuchtesensor messen
   Config Adc = Single , Prescaler = Auto , Reference = Internal_1.1
   Raw = Getadc(3)                                                              ' Dummy Messung auf MUX Kanal 3
   Waitms 1                                                                     ' mindestens 1 mS warten zwischen Einschalten ADC und GETADC
   Raw = Getadc(3)                                                              ' Messung auf MUX Kanal 3
   Stop Adc
   Reset Ubsens                                                                 ' Betriebsspannung Sensor ausschalten
   Value = 1023 - Raw
Return

' eine Nachricht senden dauert: ca. 53 bis 70 mS ohne CRC8 (32 bit)
' 3 Wiederholungen dauern: 159 bis 207 mS ohne CRC8 (32 bit) - passt fuer Sensor
' eine Nachricht senden dauert: ca. 61 bis 82 mS mit CRC8 (40 bit)
' 3 Wiederholungen dauern: 183 bis 246 mS mit CRC8 (40 bit) - passt fuer Sensor
Tx_433_send:
   Tx_dbl = 0                                                                   ' alle Bits auf 0 setzen
   'Bits uebernehmen
   Tx_byte_nr = 0
   Do                                                                           ' Sensortyp, Adresse und Werte senden
      Incr Tx_byte_nr                                                           ' beginnt mit 1
      Tx_byte = Ar(tx_byte_nr)                                                  ' Byte uebernehmen
      Gosub Tx_433_byte                                                         ' Bit 0-7 uebernehmen
   Loop Until Tx_byte_nr >= Tx_byte_anzahl                                      ' fertig

   X = 0
   Do
      ' Sync senden
      Set Tx433                                                                 ' Ausgang high
      Waitus 500                                                                ' 500 mikroS warten
      Reset Tx433                                                               ' Ausgang low
      Waitus 10000                                                              ' 10000 mikroS warten
      'Bits senden
      Y = 0                                                                     ' Beginne mit Bit 0
      Do
         If Tx_dbl.y = 1 Then                                                   ' 1 senden
            Set Tx433                                                           ' Ausgang high
            Waitus 500                                                          ' 500 mikroS warten
            Reset Tx433                                                         ' Ausgang low
            'Waitus 1000                                                         ' 1000 mikroS warten
            Waitus 920                                                          ' 920 mikroS warten, Rest braucht Programm
         Else                                                                   ' 0 senden
            Set Tx433                                                           ' Ausgang high
            Waitus 500                                                          ' 500 mikroS warten
            Reset Tx433                                                         ' Ausgang low
            'Waitus 500                                                          ' 500 mikroS warten
            Waitus 420                                                          ' 420 mikroS warten, Rest braucht Programm
         End If
         Incr Y                                                                 ' naechstes Bit
      Loop Until Y >= Tx_bit_anzahl                                             ' Ende mit gesetzter Anzahl Bits
      ' Sync senden
      Set Tx433                                                                 ' Ausgang high
      Waitus 500                                                                ' 500 mikroS warten
      Reset Tx433                                                               ' Ausgang low
      Waitus 10000                                                              ' 10000 mikroS warten
      Incr X
   Loop Until X >= Tx_repeats
Return

Tx_433_byte:
   Tx_bit_nr = Tx_byte_nr * 8                                                   ' 8, 16, 24, 32
   X = 0
   Do
      Decr Tx_bit_nr                                                            ' naechstes Bit
      Tx_dbl.tx_bit_nr = Tx_byte.x                                              ' Bit aus Byte uebernehmen
      Incr X                                                                    ' naechstes Bit
   Loop Until X >= 8                                                            ' 8 Bit
Return
