module LordOfTheRings.Hero.Cards.AragornTheTreasonOfSaruman (aragornTheTreasonOfSaruman) where

import LordOfTheRings.Helpers.RingBearer (leavesPlay)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype AragornTheTreasonOfSaruman = AragornTheTreasonOfSaruman HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Aragorn: his printed FirstPlayerGainsControl, on
-- his card def, has him follow the first player token.
aragornTheTreasonOfSaruman :: HeroCard AragornTheTreasonOfSaruman
aragornTheTreasonOfSaruman = hero AragornTheTreasonOfSaruman Cards.aragornTheTreasonOfSaruman

-- | Action: Spend 2 resources from Aragorn's resource pool to ready a hero.
instance HasAbilities AragornTheTreasonOfSaruman where
  getAbilities (AragornTheTreasonOfSaruman a) =
    [restricted a 1 (exists ExhaustedHero) $ ActionAbility (ResourceCost (HeroTarget a.id) 2)]

-- | If Aragorn leaves play, the players lose the game.
instance RunMessage AragornTheTreasonOfSaruman where
  runMessage msg h@(AragornTheTreasonOfSaruman attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select ExhaustedHero
      chooseOneM pid do
        questionLabeled "aragornTheTreasonOfSaruman.ready"
        causedBy attrs
        targets heroes ready
      pure h
    _ | leavesPlay (toTarget attrs) msg -> do
      gameOver Lost
      AragornTheTreasonOfSaruman <$> runMessage msg attrs
    _ -> AragornTheTreasonOfSaruman <$> runMessage msg attrs
