module LordOfTheRings.Attachment.Cards.PhialOfGaladriel (phialOfGaladriel) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Gift (spendBoon)
import LordOfTheRings.Attachment.Import.Lifted

newtype PhialOfGaladriel = PhialOfGaladriel AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Road Darkens. Setup: Attach to a hero in play: as the game is set
-- up, from its owner's deck (its card def's setup attachment).
phialOfGaladriel :: AttachmentCard PhialOfGaladriel
phialOfGaladriel = attachment PhialOfGaladriel Cards.phialOfGaladriel & attachTo (AttachToHero AnyHero)

-- | Action: Add Phial of Galadriel to the victory display and remove it from the campaign pool,
-- to give each enemy engaged with you -4 [attack] until the end of the round.
instance HasAbilities PhialOfGaladriel where
  getAbilities (PhialOfGaladriel a) = [restricted a 1 (exists $ EnemyEngagedWith $ PlayerWithId a.controller) $ ActionAbility Free]

instance RunMessage PhialOfGaladriel where
  runMessage msg s@(PhialOfGaladriel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      spendBoon a
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      for_ enemies \e -> roundModifier a e (AttackModifier (-4))
      pure s
    _ -> PhialOfGaladriel <$> runMessage msg a
