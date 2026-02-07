module Geom where

getAngles :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float)
getAngles playerPos botPos = do
  let (deltaX, deltaY, deltaZ) = deltas playerPos botPos

      yawRad = atan2 deltaX (-deltaY)
      yawDeg = yawRad * 180.0 / pi
      yawDeg' = if yawDeg < 0 then yawDeg + 360.0 else yawDeg

      horizontalDistance = getDistance (deltaX, deltaY)

      pitchRad = atan2 deltaZ horizontalDistance
      pitchDeg = pitchRad * 180.0 / pi
  (yawDeg', pitchDeg)

getDistance :: (Float, Float) -> Float
getDistance (deltaX, deltaY) = sqrt ((deltaX * deltaX) + (deltaY * deltaY))

deltas :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
deltas (playerX, playerY, playerZ) (botX, botY, botZ) = (botX - playerX, botY - playerY, botZ - playerZ)

degToRad :: Float -> Float
degToRad deg = deg * pi / 180.0

normalizeDelta :: Float -> Float
normalizeDelta ang
  | ang <= -pi = normalizeDelta (ang + 2 * pi)
  | ang > pi = normalizeDelta (ang - 2 * pi)
  | otherwise = ang

worldToScreen :: (Float, Float, Float) -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> Maybe (Float, Float, Float)
worldToScreen playerPos aimXDeg aimYDeg screenW screenH enemyPos =
  let (deltaX, deltaY, deltaZ) = deltas playerPos enemyPos
      horizDist = getDistance (deltaX, deltaY)
      yawToRad = atan2 deltaX (-deltaY)
      pitchToRad = atan2 deltaZ horizDist
      footPitchToRad = atan2 (deltaZ - 4.5) horizDist -- We know that the aimY normally is 4.5 by looking at its value through CE
      camYawRad = degToRad aimXDeg
      camPitchRad = degToRad aimYDeg
      deltaYaw = normalizeDelta (yawToRad - camYawRad)
      deltaPitch = normalizeDelta (pitchToRad - camPitchRad)
      footDeltaPitch = normalizeDelta (footPitchToRad - camPitchRad)
      dotProd = cos deltaYaw * cos deltaPitch
      halfHfovRad = degToRad (90 / 2)
      aspectRatio = screenH / screenW
      halfVfovRad = atan (tan halfHfovRad * aspectRatio)
      screenX = screenW / 2 + (screenW / 2) * (tan deltaYaw / tan halfHfovRad)
      screenY = screenH / 2 - (screenH / 2) * (tan deltaPitch / tan halfVfovRad)
      footScreenY = screenH / 2 - (screenH / 2) * (tan footDeltaPitch / tan halfVfovRad)
   in if notOnScreen horizDist dotProd deltaYaw deltaPitch screenX screenW screenY screenH
        then Nothing
        else Just (screenX, screenY, footScreenY)
  where
    notOnScreen horizDist dotProd deltaYaw deltaPitch screenX sw screenY sh =
      horizDist < 0.01 || dotProd < 0 || abs deltaYaw > pi / 1.8 || abs deltaPitch > pi / 1.8 || screenX < -100 || screenX > sw + 100 || screenY < -100 || screenY > sh + 100
