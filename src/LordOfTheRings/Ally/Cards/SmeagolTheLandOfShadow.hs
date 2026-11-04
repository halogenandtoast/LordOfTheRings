module LordOfTheRings.Ally.Cards.SmeagolTheLandOfShadow (smeagolTheLandOfShadow) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))

-- | Sméagol, Gollum's objective-ally side faceup (The Land of Shadow): flipped, he is the
-- enemy of "LordOfTheRings.Enemy.Cards.GollumTheLandOfShadow".
newtype SmeagolTheLandOfShadow = SmeagolTheLandOfShadow AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

smeagolTheLandOfShadow :: AllyCard SmeagolTheLandOfShadow
smeagolTheLandOfShadow = ally SmeagolTheLandOfShadow Cards.smeagolTheLandOfShadow

-- | Immune to player card effects.
instance HasModifiersFor SmeagolTheLandOfShadow where
  getModifiersFor (SmeagolTheLandOfShadow a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After the players quest unsuccessfully, flip Sméagol to Gollum.
-- If Sméagol is destroyed, the players lose the game. (This line cannot be affected by card
-- text: The Land of Shadow rules insert.)
instance HasAbilities SmeagolTheLandOfShadow where
  getAbilities (SmeagolTheLandOfShadow a) =
    [ mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After
    , mkAbility a 2 $ forced $ TargetDestroyedWindow When (toTarget a)
    ]

-- | The first player gains control of Sméagol: as the first player token passes, he goes to
-- its new holder.
instance RunMessage SmeagolTheLandOfShadow where
  runMessage msg s@(SmeagolTheLandOfShadow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    SetFirstPlayer pid | InPlayArea p <- a.placement, p /= pid -> pure $ SmeagolTheLandOfShadow $ a & placementL .~ InPlayArea pid
    _ -> SmeagolTheLandOfShadow <$> runMessage msg a
