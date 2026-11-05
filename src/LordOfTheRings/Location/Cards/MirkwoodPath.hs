module LordOfTheRings.Location.Cards.MirkwoodPath (mirkwoodPath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn, poisonedCharacters)

newtype MirkwoodPath = MirkwoodPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodPath :: LocationCard MirkwoodPath
mirkwoodPath = location MirkwoodPath Cards.mirkwoodPath

instance HasModifiersFor MirkwoodPath where
  getModifiersFor (MirkwoodPath a) = do
    total <- sum <$> (traverse poisonOn =<< poisonedCharacters)
    modifySelf a [ThreatModifier total]

instance HasAbilities MirkwoodPath where
  getAbilities (MirkwoodPath a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage MirkwoodPath where
  runMessage msg l@(MirkwoodPath a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select (HeroControlledBy AnyPlayer <> NotHero UnconsciousHero)
      chooseOrRunOneM pid do
        questionLabeled "mirkwoodPath.unconscious"
        causedBy a
        targets heroes makeUnconscious
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      poisoned <-
        selectCharacters
          (PoisonedHero <> HeroControlledBy (PlayerWithId pid))
          (PoisonedAlly <> AllyControlledBy (PlayerWithId pid))
      enemyGetsAttack a eid (length poisoned)
      pure l
    _ -> MirkwoodPath <$> runMessage msg a
