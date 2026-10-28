module LordOfTheRings.Hero.Cards.Pippin (pippin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype Pippin = Pippin HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pippin :: HeroCard Pippin
pippin = hero Pippin Cards.pippin

-- | If each hero you control has the Hobbit trait, Pippin gains: "Response: After an enemy
-- engages you, raise your threat by 3 to return it to the staging area. Until the end of
-- the round, that enemy cannot engage you."
instance HasAbilities Pippin where
  getAbilities (Pippin a) =
    [ restricted a 1 (NotCriterion $ exists $ HeroControlledBy (PlayerWithId a.owner) <> NotHero (HeroWithTrait Hobbit))
        $ ResponseAbility (PlayerEngagedEnemyWindow After a.owner) (RaiseThreatCost 3)
    ]

instance RunMessage Pippin where
  runMessage msg s@(Pippin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (headMay [eid | Window _ (EnemyEngaged eid _) <- windows]) \eid -> do
        placeEnemy eid StagingArea
        roundModifier a eid (CannotEngagePlayer pid)
      pure s
    _ -> Pippin <$> runMessage msg a
