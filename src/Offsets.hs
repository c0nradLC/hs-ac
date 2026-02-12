module Offsets where

playerEntityPointer :: Word
playerEntityPointer = 0x19d518

playerListPointer :: Word
playerListPointer = 0x19d520

maxPlayers :: Word
maxPlayers = 0x19d52C

gameMode :: Word
gameMode = 0x19d364

consumeAmmoInstr :: Word
consumeAmmoInstr = 0xfd06e

attackPhysicsFunction :: Word
attackPhysicsFunction = 0xfaf20

isVisibleFunction :: Word
isVisibleFunction = 0x1253e0

attackFunction :: Word
attackFunction = 0x78610

playerInCrosshairFunction :: Word
playerInCrosshairFunction = 0xf7780

playerHealth :: Word
playerHealth = 0x100

playerPos :: Word
playerPos = 0x8

playerAimY :: Word
playerAimY = 0x3c

playerAimX :: Word
playerAimX = 0x38

playerTeam :: Word
playerTeam = 0x320

playerState :: Word
playerState = 0x32C

playerName :: Word
playerName = 0x219

nextBot :: Word
nextBot = 0x10

codeCave :: Word
codeCave = 0x134951

dmgSubtract :: Word
dmgSubtract = 0x2fd1c

doKillFunction :: Word
doKillFunction = 0x2fe20
