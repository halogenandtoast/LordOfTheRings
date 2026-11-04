module LordOfTheRings.Ally.Cards.Abaan (abaan) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Trait (Trait (Harad))

newtype Abaan = Abaan AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, a captive of Dungeon Cell from the start (Rescue the Prisoners).
abaan :: AllyCard Abaan
abaan = ally Abaan Cards.abaan

-- | Response: After Abaan exhausts, choose a Harad character to get +1 willpower, +1
-- attack, and +1 defense until the end of the round.
-- If Abaan leaves play, the players lose the game.
instance HasAbilities Abaan where
  getAbilities (Abaan a) =
    [ mkAbility a 1 $ response $ ExhaustedWindow After (toTarget a)
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

-- | While Abaan is free of encounters, the first player gains control of Abaan: as he is
-- rescued, and as the first player token passes.
instance RunMessage Abaan where
  runMessage msg s@(Abaan a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      harad <- selectCharacters (HeroWithTrait Harad) (AllyWithTrait Harad)
      chooseOrRunOneM pid do
        questionLabeled "abaan.choose"
        causedBy a
        targets harad \c -> roundModifiers a c [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    RescueCharacter (isTarget a -> True) -> do
      lead <- getLead
      Abaan . (placementL .~ InPlayArea lead) <$> runMessage msg a
    SetFirstPlayer pid | InPlayArea _ <- a.placement -> pure $ Abaan $ a & placementL .~ InPlayArea pid
    _ -> Abaan <$> runMessage msg a
