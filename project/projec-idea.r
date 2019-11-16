# numbered variables
    # pointsPerGame
    # reboundsPerGame
# category variables
    # "guard", "forward", "center"

playerNames <- c("Klay Thompson", "Kyrie Irving", "James Harden", "Kemba Walker",
            "Khris Middleton", "Jimmy Butler", "Gordon Hayward", "Jayson Tatum",
             "Dwight Howard", "Nicola Jokic", "Nikola Vucevic", "Andre Drummond")

pointsPerGame <- c(19.5, 22.4, 24.6, 19.9,
                    15.7, 16.7, 15.2, 15.2,
                    17.2, 16.3, 15.7, 14.3)

reboundsPerGame <- c(3.5, 3.7, 5.2, 3.9, 
                       4.5, 4.9, 4.3, 5.6,
                        12.6, 9.5, 10.2, 13.8)

fieldPositions <- c("guadrd", "guard", "guard", "guard", 
                    "forward", "forward", "forward", "forward", 
                    "center", "center", "center", "center")

players <- data.frame(playerNames, pointsPerGame, reboundsPerGame, fieldPositions)




