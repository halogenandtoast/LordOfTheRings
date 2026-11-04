module LordOfTheRings.Hero.Cards.AragornTheMountainOfFire (aragornTheMountainOfFire) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype AragornTheMountainOfFire = AragornTheMountainOfFire HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel. The first player gains control of Aragorn: his FirstPlayerGainsControl
-- keyword.
aragornTheMountainOfFire :: HeroCard AragornTheMountainOfFire
aragornTheMountainOfFire = hero AragornTheMountainOfFire Cards.aragornTheMountainOfFire

-- | Action: Spend 1 resource from Aragorn's pool to ready him. (Limit once per phase.)
instance HasAbilities AragornTheMountainOfFire where
  getAbilities (AragornTheMountainOfFire a) =
    [ limited (PlayerLimit PerPhase 1)
        $ restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero)
        $ ActionAbility (ResourceCost (toTarget a) 1)
    ]

-- | If Aragorn leaves play, the players lose the game.
instance RunMessage AragornTheMountainOfFire where
  runMessage msg h@(AragornTheMountainOfFire attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ | leavesPlay (toTarget attrs) msg -> do
      gameOver Lost
      pure h
    _ -> AragornTheMountainOfFire <$> runMessage msg attrs
