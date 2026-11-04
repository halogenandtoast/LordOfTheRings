module LordOfTheRings.Location.Cards.ShrineToMorgothTheDrownedRuins (shrineToMorgothTheDrownedRuins) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

-- | The Drowned Ruins' Shrine to Morgoth, Grotto side faceup: its other side is
-- "LordOfTheRings.Location.Cards.ShrineToMorgothUnderwater".
newtype ShrineToMorgothTheDrownedRuins = ShrineToMorgothTheDrownedRuins LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

shrineToMorgothTheDrownedRuins :: LocationCard ShrineToMorgothTheDrownedRuins
shrineToMorgothTheDrownedRuins = location ShrineToMorgothTheDrownedRuins Cards.shrineToMorgothTheDrownedRuins

-- | Shrine to Morgoth cannot have attachments and cannot enter the staging area.
instance HasModifiersFor ShrineToMorgothTheDrownedRuins where
  getModifiersFor (ShrineToMorgothTheDrownedRuins a) = modifySelf a [CannotHaveAttachments]

-- | Forced: At the beginning of the quest phase, flip Shrine to Morgoth to its Underwater
-- side, without removing its progress tokens.
instance HasAbilities ShrineToMorgothTheDrownedRuins where
  getAbilities (ShrineToMorgothTheDrownedRuins a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

-- | When Shrine to Morgoth is placed in the victory display, the players have escaped from
-- the flooded grotto and win the game.
instance RunMessage ShrineToMorgothTheDrownedRuins where
  runMessage msg l@(ShrineToMorgothTheDrownedRuins a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOverKeepingProgress a.id
      pure l
    AddToVictoryDisplay (isTarget a -> True) -> do
      winGame
      ShrineToMorgothTheDrownedRuins <$> runMessage msg a
    _ -> ShrineToMorgothTheDrownedRuins <$> runMessage msg a
