
(*Based on the SATURN V rocket*)

val standardGravity = 9.80665 (*m/s^2*)

val earthsMeanRadius = 6371000.0 (*meters*)

val fuelExpulsionPerF1ThrusterPerSecond = 1789.0(*liquid oxygen*) + 788.0(*RP-1*) (*kg*)
(*http://en.wikipedia.org/wiki/Rocketdyne_F-1*)

val f1_WeightOfThrusters = 8391.0 (*kg*)
val f1_ThrustPerSecond = 6700000.0 (*Newtons*)

val thrustF1Force = 6700000.0

val weightOfSaturnVBeforeLaunch = 2766913.0 (*kg*)


(*
 gravityPullForce (altitude,mass)
 TYPE: real * real => real
 PRE: true
 POST: Calculation of gravitational force on aircraft depending on altitude and mass.
 SI-UNIT: Newtons
 EXAMPLES: gravityPullForce(50000.0, 30000.0) = 289635.5062
*)
fun gravityPullForce (altitude,mass) = mass * standardGravity * (earthsMeanRadius / (earthsMeanRadius + altitude)) * (earthsMeanRadius / (earthsMeanRadius + altitude))

fun testgravityPullForce (altitude) = standardGravity * (earthsMeanRadius / (earthsMeanRadius + altitude)) * (earthsMeanRadius / (earthsMeanRadius + altitude))


(*******************************************************************************************************************************************************)
(*********************************************************************** CONSTANTS *********************************************************************)
(*******************************************************************************************************************************************************)

(* 
 Sea level standard atmospheric pressure in Pa
*)

val standardAtmosphericPressureSeaLevel = 101325.00

(*
 Sea level standard temperature in Kelvin
*)

val standardTemperatureSeaLevel = 288.15


(*
 Temperature lapse rate in Kelvin/meter
*)

val temperatureLapseRate = 0.0065

(*
 Gas constant in Joule/(Mol * Kelvin)
*)

val gasConstant = 8.31447

(*
 Molar mass of dry air in kg/mol
*)

val molarMassDryAir = 0.0289644

(*
 Temperature in the lower stratosphere (11000 m - 25100 m) given in kelvin 
*)

val temperatureAtLowerStratosphere = 216.483

(*
 dragCoefficient of a standard rocket. 
*)

val dragCoefficient = 0.75

(*******************************************************************************************************************************************************)
(******************************************************************** AIR RESISTANCE *******************************************************************)
(*******************************************************************************************************************************************************)

(*
 lapseRate altitude
 TYPE: real => real
 PRE: altitude > 0.0
 POST: Lapse rate (change in temperature) of the atmosphere depending on altitude
 EXAMPLE: lapseRate(60000.0) = 0.0028
*)

fun lapseRate altitude = 
    if 0.0 <= altitude andalso altitude < 11000.0 then
	0.0065
    else if 11000.0 <= altitude andalso altitude < 20000.0 then
	0.0
    else if 20000.0 <= altitude andalso altitude < 32000.0 then
	~0.001
    else if 32000.0 <= altitude andalso altitude  < 47000.0 then
	~0.0028
    else if 47000.0 <= altitude andalso altitude  < 51000.0 then
	0.0
    else if 51000.0 <= altitude andalso altitude  < 71000.0 then
	0.0028
    else
	0.0020



(*
 temperatureAtHigherStratosphere altitude
 TYPE: real => real
 PRE: 25000.0 < altitude < 86000.0   
 POST: Temperature in the higher stratosphere (25000m - 86000m) at altitude meters.
 SI-UNIT: Kelvin
 EXAMPLE: temperatureAtHigherStratosphere(30000.0) = 231.64
*)


fun temperatureAtHigherStratosphere altitude = (~131.21 + (0.00299 * altitude)) + 273.15  (*temperature in kelvin up to 50 000m*)


(*
 temperatureAtAltitude altitude
 TYPE: real => real
 PRE: altitude > 0.0
 POST: Temperature at altitude meters
 EQUATION: Aboslute temperature = standardTemperatureSeaLevel - (temperatureLapseRate * altitude)
 SI-UNIT: Kelvin
 EXAMPLE: temperatureAtAltitude(5000.0) = 255.65
*)

fun temperatureAtAltitude (altitude) = 
    if altitude < 11000.0 then
	standardTemperatureSeaLevel - (lapseRate(altitude) * altitude)
    else if altitude > 11001.0 andalso altitude < 25000.0 then
	temperatureAtLowerStratosphere
    else
	temperatureAtHigherStratosphere(altitude)



(*
 airPressureTroposphere altitude
 TYPE: real => real
 PRE: 0.0 < altitude < 11000.0
 POST: Atmospheric pressure in the troposphere (0.0 - 11000.0) at altitude meters
 EQUATION: pressure = standardAtmosphericPressureSeaLevel(1 - (temperatureLapseRate * altitude)/standardTemperatureSeaLevel)^((gravity*molarMassDryAir)/(gasConstant*temperatureLapseRate))
 SOURCE: http://en.wikipedia.org/wiki/Density_of_air#Altitude
 SI-UNIT: Pascal
 EXAMPLE: airPressureTroposphere(2000.0) = 79495.56349
*)

fun airPressureTroposphere (altitude) = standardAtmosphericPressureSeaLevel *(Math.pow((1.0 - ((lapseRate(altitude) * altitude)/(standardTemperatureSeaLevel))), ((standardGravity * molarMassDryAir) / (gasConstant * temperatureLapseRate))))


(*
 pressureLowerStratosphere altitude
 TYPE: real => real
 PRE: 11000.0 <= altitude < 25000.0
 POST: Atmospheric pressure in the lower stratosphere (11000.0 m - 25000.0 m) at altitude 
 SI-UNIT: Pascal
 EXAMPLE: pressureLowerStratosphere(15000.0) = 12123.67136
*)

fun pressureLowerStratosphere (altitude) = (22.65 * Math.exp (1.73 - (0.000157 *   ((altitude))))) * 1000.0


(*
 pressureHigherStratosphere altitude
 TYPE: real => real
 PRE: 25000.0 <= altitude < 86000.0
 POST: Atmospheric pressure in the upper stratosphere (25000 m - 86000 m) at altitude.
 SI-UNIT: Pascal
 EXAMPLE: pressureHigherStratosphere(30000.0) = 1158.329327
*)

fun pressureHigherStratosphere (altitude) = (2.488 * (Math.pow(((temperatureAtAltitude(altitude)) /  216.6), ~11.388)) * 1000.0)

(*
 airPressureAtAltitude altitude
 TYPE: real => real
 PRE: altitude > 0.0
 POST: Atmospheric pressure at altitude.
 SI-UNIT: Pascal
 EXAMPLE: airPressureAtAltitude(10000.0) = 26436.90841
*)

 
fun airPressureAtAltitude (altitude) = 
    if altitude < 11000.0 then
 	airPressureTroposphere(altitude)
    else if altitude >= 11000.0 andalso altitude < 25000.0 then
	pressureLowerStratosphere(altitude)
    else if altitude >= 25000.0 andalso altitude < 86000.0 then
	pressureHigherStratosphere(altitude)
    else
	0.0

(*
 airDensityAtAltitude altitude
 TYPE: real => real
 PRE: altitude > 0.0, temperatureAtAltitude > 0.0
 POST: The air density at altitude meters.
 EQUATION: density = (airPressureAtAltitude * molarMassDryAir) / (gasConstant * temperatureAtAltitude)
 SI-UNIT: kg/m^3
 EXAMPLE: airDensityAtAltitude(9000.0) = 0.4663494352
*)

fun airDensityAtAltitude (altitude) = (airPressureAtAltitude(altitude) * molarMassDryAir) / (gasConstant * temperatureAtAltitude(altitude))

(*
 crossSectionalArea diameter
 TYPE: real => real
 PRE: diameter > 0.0
 POST: Cross-sectional area of object calculated from diameter.
 EQUATION: Cross section = (diameter^2 * pi / 4)
 SI-UNIT: m^2
 EXAMPLE: crossSectionalArea(10.0) = 78.53981634
*)

fun crossSectionalArea (diameter) = (Math.pow(diameter, 2.0) * Math.pi) / 4.0


(*
 preVelocityVector velocityX, velocityY
 TYPE: real * real => real
 PRE: True
 POST: Magnitude of velocity vector depending on velocityX and velocityY.
 EQUATION: Sqrt(Vx^2 + Vy^2)
 SI-UNIT: m/s
 EXAMPLE: preVelocityVector(200.0, 100.0) = 223.6067977
*)

fun preVelocityVector (Vx, Vy) = Math.sqrt((Math.pow(Vx, 2.0)) + (Math.pow(Vy, 2.0)))

(*
 airResistance (altitude, verticalVelocity, horizontalVelocity, diameter)
 TYPE: real * real * real * real => real
 PRE: diameter > 0.0, altitude > 0.0
 POST: Air-resistance force of an object depending on altitude, verticalVelocity, horizontalVelocity and diameter of object.
 SI-UNIT: Newtons
 EQUATION: Air resistance force = 1/2 * air resistance coefficient * density of air (depending on height) * Cross-sectional area of object * (The speed of the rocket relative to the air)^2
 EXAMPLE: airResistance(3000.0, 200.0, 100.0, 10.0) = 1338776.597
*)

fun airResistance (altitude, Vx, Vy, diameter) = (airDensityAtAltitude(altitude) * (Math.pow(preVelocityVector(Vx, Vy), 2.0)) * crossSectionalArea(diameter) * dragCoefficient) / 2.0


(*******************************************************************************************************************************************************)
(*********************************************************************** FORCE *************************************************************************)
(*******************************************************************************************************************************************************)


(*
 thrustForce thrusters
 TYPE: real => real
 PRE: True
 POST: Total thrust force of an object depending on number of thrusters used.
 SI-UNIT: Newtons
 EXAMPLE: thrustForce(3.0) = 20100000.0 
*)

fun thrustForce (thrusters) = (thrustF1Force * thrusters)

(*
 totalForceX altitude, verticalVelocity, horizontalVelocity, diameter, angle, thrusters
 TYPE: real * real * real * real * real * real => real
 PRE: angle given in radians
 POST: Net force of an object on the horizontal axis depending on altitude, verticalVelocity, horizontalVelocity, diameter, thrusters and angle.
 SI-UNIT: Newtons
 EQUATION: (Thrust force - air resistance) * cos(angle)
 EXAMPLE: totalForceX(20000.0, 300.0, 100.0, 10.0, Math.pi/4.0, 5.0) = 23502755.88
*)

fun totalForceX (altitude, Vx, Vy, diameter, preAngle, thrusters) = (thrustForce(thrusters) - airResistance(altitude, Vx, Vy, diameter)) * Math.cos(preAngle)

(*
 totalForceY altitude, verticalVelocity, horizontalVelocity, diameter, angle, thrusters
 TYPE: real * real * real * real * real * real => real
 PRE: angle given in radians
 POST: Net force of an object on the vertical axis depending on altitude, verticalVelocity, horizontalVelocity, diameter, thrusters and angle.
 SI-UNIT: Newtons
 EQUATION: ((Thrust force - air resistance) * sin(angle)) - gravity
 EXAMPLE: totalForceY(30000.0, 100.0, 500.0, 10.0, Math.pi/2.0, 3000000.0, 5.0) = 4214437.2
*)

fun totalForceY (altitude, Vx, Vy, diameter, preAngle, mass, thrusters) = ((thrustForce(thrusters) - airResistance(altitude, Vx, Vy, diameter)) * Math.sin(preAngle)) - gravityPullForce(altitude, mass)

(*******************************************************************************************************************************************************)
(******************************************************************** ACCELERATION *********************************************************************)
(*******************************************************************************************************************************************************)


(*
 accX (altitude, mass, thrusters, verticalVelocity, horizontalVelocity, diameter, fuel, angle)
 TYPE: real * real * real * real * real * real * real * real * real * real => real
 PRE: angle given in radians and mass > 0.0
 POST: If fuel > 0.0 then the acceleration, accX, on the horizontal axis is given from totalForceX depending on the values of altitude, mass, thrusters, verticalVelocity,
       horizontalVelocity, diameter, fuel and angle. Else the negative value of airResistance.
 SI-UNIT: m/s^2
 EQUATION: Acceleration = Netforce / Mass
 EXAMPLE: accX(1000.0, 3000000.0, 5.0, 300.0, 200.0, 10.0, 2000000.0, Math.pi/3.0) = 4.8739
*)

fun accX (Y, mass, thrusters, Vx, Vy, diameter, fuel, preAngle) = 
    if fuel > 0.0 then
	totalForceX(Y, Vx, Vy, diameter, preAngle, thrusters) / mass
    else
	0.0 - ((airResistance(Y, Vx, Vy, diameter) * Math.cos(preAngle)) / mass)

(*
 accY altitude, mass, thrusters, verticalVelocity, horizontalVelocity, diameter, fuel, angle
 TYPE: real * real * real * real * real * real * real * real * real * real => real
 PRE: angle given in radians and mass > 0.0
 POST: If fuel > 0.0 then the acceleration, accY, on the vertical axis is given from totalForceY depending on the values of altitude, mass, thrusters, verticalVelocity,
       horizontalVelocity, diameter, fuel and angle. Else the negative force of airResistance + gravityPullForce.
 SI-UNIT: m/s^2
 EQUATION: Acceleration = Netforce / Mass
 EXAMPLE: accY(20000.0, 2000000.0, 5.0, 1000.0, 0.0, 10.0, 200000.0, Math.pi/3.0) = 3.625
*)

fun accY (Y, mass, thrusters, Vx, Vy, diameter, fuel, preAngle) =
    if fuel > 0.0 then
	totalForceY(Y, Vx, Vy, diameter, preAngle, mass, thrusters) / mass
    else
	0.0 - ((gravityPullForce(Y, mass) + (airResistance(Y, Vx, Vy, diameter) * Math.sin(preAngle))) / mass)


(*******************************************************************************************************************************************************)
(**************************************************************** ANGLE CONVERSION *********************************************************************)
(*******************************************************************************************************************************************************)

(*
 degreesToRadians degrees
 TYPE: real => real
 PRE: True
 POST: degrees converted to radians.
 EXAMPLE: degreesToRadians(180.0) = 3.141592654 (pi)
*)

fun degreesToRadians (degrees) = (degrees * Math.pi)/180.0

(*
 radiansToDegrees radians
 TYPE: real => real
 PRE: True
 POST: radians converted to degrees.
 EXAMPLE: degreesToRadians(180.0) = 3.141592654 (pi)
*)

fun radiansToDegrees (radians) = (radians * 180.0) / Math.pi 

(*******************************************************************************************************************************************************)
(**************************************************************** DISTANCE AND FUEL ********************************************************************)
(*******************************************************************************************************************************************************)

(*
 distance (acceleration, velocity, altitude, time)
 TYPE: real * real * real * real => real
 PRE: time > 0.0
 POST: The distance travelled from last time increment based on acceleration, velocity and altitude.
 SI-UNIT: Meters
 EQUATION: Previous altitude + (Velocity * Change_in_time) + (0.5 * Acceleration * Change_in_time)
 EXAMPLE: distance(4.0, 200.0, 1000.0, 1.0) = 1202.0
*)

fun distance (ac,velo,preAlt,chT) = preAlt + (velo * chT) + (0.5 * ac * chT)

(*
 remainingFuelCalculator (fuel, numberOfThrusters, time, kgFuelPerSecond)
 TYPE: real * real * real * real => real
 PRE: fuel > 0.0, numberOfThrusters > 0.0, kgFuelPerSecond > 0.0
 POST: If fuel > 0.0, then new fuel remaining for this time interval depending on numberOfThrusters and kgFuelPerSecond
       else ~0.1
 SI-UNIT: Kilograms
 EXAMPLE: remainingFuelCalculator(250000.0, 5.0, 1.0, 2000.0) = 240000.0
*)
									
fun remainingFuelCalculator (fuelLeft,nrOfThrusters,changeTime,kgFuelPerSecond) =
    if fuelLeft < ((kgFuelPerSecond * nrOfThrusters * changeTime)) then 
	~0.1
    else
	fuelLeft - ((kgFuelPerSecond * nrOfThrusters * changeTime))

(*
 remainingMassCalculator (mass, fuel, numberOfThrusters, time, kgFuelPerSecond)
 TYPE: real * real * real * real * real => real
 PRE: mass > 0.0, fuel > 0.0, numberOfThrusters > 0.0, kgFuelPerSecond > 0.0, mass > fuel
 POST: If fuel > 0.0, then mass, else mass - fuel burnt for that time interval (depending on numberOfThrusters, time and kgFuelPerSecond).
 EXAMPLE: remainingMassCalculator (2000000.0, 1000000.0, 5.0, 1.0, 2000.0) = 1990000.0
*)
	       
fun remainingMassCalculator (mass,fuelLeft,nrOfThrusters,changeTime,kgFuelPerSecond) =
    if fuelLeft < 0.0 then 
	mass
    else
	mass - ((kgFuelPerSecond * nrOfThrusters * changeTime))	
		   
(* 
 launcher (diameter, mass, thrusters, fuel, interval, angle)
 TYPE: real * real * real * real * int * real -> (real * real * real * real * real * real * int * real * real) list
 PRE: diameter > 0.0, interval > 0, mass > 0.0
 POST: Initiates auxLauncher with the user set values diamater, mass, fuel, interval and angle. Returns value list when auxLauncher terminates.
 EXAMPLE: launcher(10.0, 731800.0, 5.0, 2169000.0, 1, 65.0) = 
    [(1.060681007E~15, 2.612832501, 7.071206711E~16, 1.741888334,
     7.071206711E~16, 1.741888334, 1, 2156115.0, 1.570796327),
    (2.83321164E~15, 7.044797666, 1.41739398E~15, 3.535272888,
     7.102733084E~16, 1.793384554, 2, 2143230.0, 1.570796327),
    (5.320779523E~15, 13.34797148, 2.130843248E~15, 5.38054017,
     7.134492689E~16, 1.845267282, 3, 2130345.0, 1.570796327),
    (8.526595443E~15, 21.57481409, 2.847491696E~15, 7.278075131,
     7.166484474E~16, 1.897534962, 4, 2117460.0, 1.570796327),
    (1.245389323E~14, 31.77816799, 3.567362423E~15, 9.228260978,
     7.198707271E~16, 1.950185846, 5, ...),
    (1.710592962E~14, 44.01125598, 4.290478403E~15, 11.23147898,
     7.231159798E~16, 2.003218006, ...),
    (2.248598412E~14, 58.32767897, 5.016862469E~15, 13.28810832,
     7.263840667E~16, ...),
    (2.859735885E~14, 74.78141367, 5.746537309E~15, 15.39852591, ...),
    (3.544337837E~14, 93.42681009, 6.47952545E~15, ...),
    (4.302738954E~14, 114.318589, ...), ...]:
*)
(*******************************************************************************************************************************************************)
(********************************************************************** LAUNCHER ***********************************************************************)
(*******************************************************************************************************************************************************)

fun launcher (diameter, mass, thrusters, fuel, angle) =
    let
	val initialAngle = Math.pi/2.0
	val initialVelocityX = 0.0
	val interval = 1
	val initialVelocityY = 0.0
	val initialTotalDisplacementX = 0.0
	val initialTotalAltitudeY = 0.0
	val initialTime = 0
	val totalMass = mass + fuel
	val angleInRadians = degreesToRadians(angle)

       (*
	angleCheck angle
        TYPE: real => real
        PRE: True
        POST: Checks whether or not angle is greater than Pi/2, if so it adds an insignificant value to Pi/2 else substracts an insignificant value from Pi/2.
        EXAMPLE: angleCheck(Math.pi/3.0) = 1.04619755
       *)

	fun angleCheck (angle') =
	    if angle' > (Math.pi/2.0) then
		initialAngle - 0.001
	    else
		initialAngle + 0.001

	(*
	 gradualChange angle
         TYPE: real => real
         PRE: True
         POST: Gradually increases or decreases local angle until it equals launchers' angle.
         EXAMPLE: gradualChange(Math.pi/3.0) = 1.55079632
        *)

	fun gradualChange (angle') = 
	    if angle' < angleCheck(angleInRadians) then
		if angle' > (angleInRadians + 0.03) then
		    (angle' - 0.008)
		else
		    angleInRadians
	    else
		if angle' < (angleInRadians - 0.03) then
		    (angle' + 0.008)
		else
		    angleInRadians
			
	(*
         flightAngle (altitude, accelerationY, velocityX, velocityY, angle)
         TYPE: real * real * real * real => real
         PRE: altitude > 0.0, velocityX =/= 0.0
         POST: If altitude < 100.0 then value initialAngle (Math.pi/2.0). 
               If acceleration is positive when altitude > 100.0 then new flight angle based on launchers' angle, else arctan(velocityY/velocityX). 
         EXAMPLE: flightAngle(1000.0, 2.0, 100.0, 100.0) = 
	*)

	fun flightAngle (Y, accY, Vx, Vy, angle') = 
	    if accY > 0.1 then
		if Y > 100.0 then
		    gradualChange(angle')
		else
		    initialAngle
	    else
		Math.atan(Vy/Vx)

	(*
	 auxLauncher (diameter, mass, thrusters, fuel, changeInTime, velocityX, velocityY, totalDisplacementX, totalAltitudeY, time, accumulator, angle)
	 TYPE: real * real * real * real * int * real * real * real * real * int * real list * real => (real * real * real * real * real * real * int * real * real) list
	 PRE: diameter > 0.0, mass > 0.0, fuel > 0.0
	 POST: Calculates new values for the current time interval during flight (totalAltitudeY > 0.0) regarding mass, fuel, velocityX, velocityY, totalDisplacementX, totalAltitudeY and angle with               the constants diameter and thrusters. Then appends them with accumulator when altitudeY < 0.0.
	 EXAMPLE: auxLauncher(10.0, 731800.0, 5.0, 2169000.0, 1, 0.0, 0.0, 0.0, 0.0, 1, [], 65.0) = 
                  [(1.060681007E~15, 2.612832501, 7.071206711E~16, 1.741888334,
                    7.071206711E~16, 1.741888334, 1, 2156115.0, 1.570796327),
                   (2.83321164E~15, 7.044797666, 1.41739398E~15, 3.535272888,
                    7.102733084E~16, 1.793384554, 2, 2143230.0, 1.570796327),
                   (5.320779523E~15, 13.34797148, 2.130843248E~15, 5.38054017,
                    7.134492689E~16, 1.845267282, 3, 2130345.0, 1.570796327),
                   (8.526595443E~15, 21.57481409, 2.847491696E~15, 7.278075131,
                    7.166484474E~16, 1.897534962, 4, 2117460.0, 1.570796327),
                   (1.245389323E~14, 31.77816799, 3.567362423E~15, 9.228260978,
                    7.198707271E~16, 1.950185846, 5, ...),
                   (1.710592962E~14, 44.01125598, 4.290478403E~15, 11.23147898,
                    7.231159798E~16, 2.003218006, ...),
                   (2.248598412E~14, 58.32767897, 5.016862469E~15, 13.28810832,
                    7.263840667E~16, ...),
                   (2.859735885E~14, 74.78141367, 5.746537309E~15, 15.39852591, ...),
                   (3.544337837E~14, 93.42681009, 6.47952545E~15, ...),
                   (4.302738954E~14, 114.318589, ...), ...]:
	 *)

	fun auxLauncher (diameter',mass',thrusters',fuel',changeInTime',velocityX', velocityY',totalDisplacementX', totalAltitudeY', time', acc', angle') =
	    let
		val newAccelerationX = accX(totalAltitudeY', mass', thrusters', velocityX', velocityY', diameter', fuel', angle')
		val newAccelerationY = accY(totalAltitudeY', mass', thrusters', velocityX', velocityY', diameter', fuel', angle')
		val newVelocityX = velocityX' + (newAccelerationX * Real.fromInt(changeInTime'))			(*prevV + acc * deltaT*)
		val newVelocityY = velocityY' + (newAccelerationY * Real.fromInt(changeInTime'))
		val newFlightAngle = flightAngle(totalAltitudeY', newAccelerationY, velocityX', velocityY', angle')
		val newDisplacementX = distance(newAccelerationX, newVelocityX, totalDisplacementX', Real.fromInt(changeInTime'))
		val newAltitudeY = distance(newAccelerationY, newVelocityY, totalAltitudeY', Real.fromInt(changeInTime'))
		val remainingFuel = remainingFuelCalculator(fuel',thrusters',Real.fromInt(changeInTime'),fuelExpulsionPerF1ThrusterPerSecond)
		val newMass = remainingMassCalculator(mass',fuel',thrusters',Real.fromInt(changeInTime'),fuelExpulsionPerF1ThrusterPerSecond)
		val newTime = time' + changeInTime'
		val flightData = (newDisplacementX, newAltitudeY, newVelocityX, newVelocityY, newAccelerationX, newAccelerationY, newTime, remainingFuel, newFlightAngle)

	    in
		if newAltitudeY < 0.0 then
		    List.rev(acc')
		else
		    auxLauncher(diameter', newMass, thrusters', remainingFuel, changeInTime', newVelocityX, newVelocityY, newDisplacementX, newAltitudeY, newTime, flightData::acc', newFlightAngle)
	    end

    in auxLauncher (diameter,totalMass,thrusters,fuel,interval,initialVelocityX, initialVelocityY,initialTotalDisplacementX, initialTotalAltitudeY,initialTime,[], initialAngle)

    end





(*
 discardInsignificantValues value
 TYPE: real => real
 PRE: True
 POST: Discards and sets values deemed insignificant i.e. ~0.1 < value < 0.1 to 0.0 else value
 EXAMPLES: discardInsignificantValues(0.0942) = 0.0
           discardInsignificantValues(3.4) = 3.4
*)

fun discardInsignificantValues (value) = 
    if value < 0.1 andalso value > ~0.1 then
	0.0
    else
	value

(*******************************************************************************************************************************************************)
(******************************************************************* SIMULATEDLAUNCH *******************************************************************)
(*******************************************************************************************************************************************************)


(*
 simulatedLaunch2 (tupleList, sleepInt, infoInterval)
 TYPE: (real * real * real * real * real * real * int * real * real) list * int * int => unit
 PRE: sleepInt >= 0, infoInterval > 0
 POST: ()
 SIDE-EFFECTS: Prints the initiate sequence and countdown with the sleep time sleepInt executed between each new line. Passes the values tupleList, sleepInt and infoInterval to the helper function simLaunch.
 EXAMPLE: simulatedLaunch2([(1.060681007E~15, 2.612832501, 7.071206711E~16, 1.741888334, 7.071206711E~16, 1.741888334, 1, 2156115.0, 1.570796327)], 1, 1) =
 
          Initiating launch sequence...
          Countdown: 3...
          Countdown: 2...
          Countdown: 1...

          Seconds after launch: 1
          Your altitude is 2.61283250138 meters.
          Your displacement is 0.0 meters.
          Your vertical speed is 1.74188833425 m/s.
          Your horizontal speed is 0.0 m/s. 
          Your vertical acceleration is 1.74188833425 m/s^2.
          Your horizontal acceleration is 0.0 m/s^2.
          Flying at an angle of 90.0 degrees.
          Remaining fuel: 2156115.0kg.
          Awaiting update...


          Mission Accomplished! You reached: 2.612832501 meters.
*)

fun simulatedLaunch (list,sleepInt,infoInterval) =
    let
	(*
	 simLaunch ((disX, altY, velX, velY, accX, accY,time,fuel, angle)::rest as tupleList, previousHeight, peakHeight, sleepint, infoInterval)
	 TYPE: (real * real * real * real * real * real * int * real * real) list * real * real * int * int => unit
	 PRE: sleepint >= 0, infoInterval > 0
	 POST: ()
	 SIDE-EFFECTS: Prints the first tuple in tupleList with befittingly strings for each infoInterval. Between every recursive call the sleep time sleepInt is executed and maximum altitude altY                       assigned to peakHeight. When tupleList is empty, peakHeight is printed. 
	 VARIANT: length of list
	 EXAMPLE: simLaunch([(0.0, 10.0, 0.0, 8.0, 0.0, 2.0, 1, 2000000.0, Math.pi/2.0), (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2, 2000000.0, Math.pi/2.0], 0.0, 10.0, 1, 1) =

                  Seconds after launch: 1
                  Your altitude is 10.0 meters.
                  Your displacement is 0.0 meters.
                  Your vertical speed is 8.0 m/s.
                  Your horizontal speed is 0.0 m/s.
                  Your vertical acceleration is 2.0 m/s^2.
                  Your horizontal acceleration is 0.0 m/s^2.
                  Flying at an angle of 90.0 degrees.
                  Remaining fuel: 2000000.0kg.
                  Awaiting update... 

                  Seconds after launch: 2
                  Your altitude is 0.0 meters.
                  Your displacement is 0.0 meters.
                  Your vertical speed is 0.0 m/s.
                  Your horizontal speed is 0.0 m/s.
                  Your vertical acceleration is 0.0 m/s^2.
                  Your horizontal acceleration is 0.0 m/s^2.
                  Flying at an angle of 90.0 degrees.
                  Remaining fuel: 2000000.0kg.
                  Awaiting update... 

                  Mission Accomplished! You reached: 10.0 meters.
	*)
	
	fun simLaunch ([],prevHeight,peakHeight,sleepInt',infoInterval') = (print "\n";print ("Mission Accomplished! You reached: " ^ Real.toString(peakHeight) ^ " meters.");print "\n")
	  | simLaunch ((disX, altY, velX, velY, accX, accY,time,fuel, angle)::rest,prevHeight,peakHeight,sleepInt',infoInterval') =
	    if time mod infoInterval' = 0 then
					     let 
						 val _ = Posix.Process.sleep (Time.fromSeconds (sleepInt'))
						 val _ = print ("Seconds after launch: " ^ (Int.toString time))
						 val _ = print "\n"
						 val _ = print ("Your altitude is " ^ (Real.toString altY) ^ " meters.")
						 val _ = print "\n"
						 val _ = print ("Your displacement is " ^ (Real.toString(discardInsignificantValues(disX))) ^ " meters.")
						 val _ = print "\n"
						 val _ = print ("Your vertical speed is " ^ (Real.toString velY) ^ " m/s.")
						 val _ = print "\n"
						 val _ = print ("Your horizontal speed is " ^ (Real.toString(discardInsignificantValues(velX))) ^ " m/s.")
						 val _ = print "\n"
						 val _ = print ("Your vertical acceleration is " ^ (Real.toString accY) ^ " m/s^2.")
						 val _ = print "\n"
						 val _ = print ("Your horizontal acceleration is " ^ (Real.toString(discardInsignificantValues(accX))) ^ " m/s^2.")
						 val _ = print "\n"
						 val _ = print ("Flying at an angle of " ^ (Real.toString(radiansToDegrees(angle))) ^ " degrees.")
						 val _ = print "\n"
						 val _ = print ("Remaining fuel: " ^ (Real.toString fuel) ^ "kg.")
						 val _ = print "\n"
						 val _ = print ("Awaiting update... ")
						 val _ = print "\n"
						 val _ = print "\n"
					     in
						 if altY > prevHeight then
						     simLaunch (rest,altY,altY,sleepInt',infoInterval')
						 else
						     simLaunch(rest,altY,peakHeight,sleepInt',infoInterval')
					     end
	    else
		if altY > prevHeight then
		    simLaunch (rest,altY,altY,sleepInt',infoInterval')
		else
		    simLaunch(rest,altY,peakHeight,sleepInt',infoInterval')
    in
	let
	    val _ = print ("Initiating launch sequence...")
	    val _ = print ("\n")
	    val _ = Posix.Process.sleep (Time.fromSeconds 3)
	    val _ = print ("Countdown: 3...")
	    val _ = print ("\n")
	    val _ = Posix.Process.sleep (Time.fromSeconds 1)
	    val _ = print ("Countdown: 2...")
	    val _ = print ("\n")
	    val _ = Posix.Process.sleep (Time.fromSeconds 1)
	    val _ = print ("Countdown: 1...")
	    val _ = print ("\n")
	    val _ = Posix.Process.sleep (Time.fromSeconds 1)
	    val _ = print ("\n")
	in simLaunch (list,0.0,0.0,sleepInt,infoInterval)
	end
    end


(*******************************************************************************************************************************************************)
(********************************************************************* TEST CASES *********************************************************************)
(*******************************************************************************************************************************************************)


(*
Typical user inputs:
*)
val automated_test_1 = (length(launcher(10.0,731800.0,5.0,2169000.0, 65.0)) = 556)


(*
Typical user values inputs:
*)
val automated_test_2 = (length(launcher(3.0,60000.0,5.0,2169000.0, 85.0)) = 4768)


(* 
Extreme values for fuel input:
*)
val automated_test_3 = (length(launcher(10.0,731800.0,5.0,9000000000.0, 65.0)) = 0)


(*
extreme value for the diameter of the rocket and angle of trajectory:
*)
val automated_test_4 = (length(launcher(0.0,731800.0,50.0,2169000.0, 180.0)) = 791)


(*
Extreme value for rocket diameter:
*)
val automated_test_5 = (length(launcher(1000.0,731800.0,5.0,2169000.0, 65.0)) = 59)



(*
Extreme value for nr of thruster used to simulate the rocket and diameter of rocket:
*)
val automated_test_6 = (length(launcher(0.0,731800.0,600.0,2169000.0, 65.0)) = 1103)

(*
Extreme value for angle of trajectory in degrees and diameter of rocket:
*)
val automated_test_7 = (length(launcher(0.0,731800.0,5.0,2169000.0, 1080.0)) = 390)



(*
Extreme value for angle of trajectory in degrees and diameter of rocket:
*)
val automated_test_8 = (length(launcher(0.01,731800.0,5.0,2169000.0, ~110.0)) = 390)


(*
Extreme values for fuel,thrusters and diameter of rocket:
*)
val automated_test_9 = (length(launcher(0.0,731800.0,550.0,216900000000.0, 90.0)) = 0)


(*
Extreme values for angle of trajectory:
*)
val automated_test_10 = (length(launcher(5.0,731800.0,5.0,2169000.0, 0.01)) = 387 )


(*
Uncomment this and run testSaturnV to do a simulation of the rocket Saturn V.
*)
val SaturnV = launcher(10.0,731800.0,5.0,2169000.0, 120.0) (*Estimated values for the real rocket Saturn V, adjusted to our simulator.*)
fun testSaturnV () = simulatedLaunch(SaturnV,2, 1);

