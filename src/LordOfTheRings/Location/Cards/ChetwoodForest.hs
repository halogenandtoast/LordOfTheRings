module LordOfTheRings.Location.Cards.ChetwoodForest (chetwoodForest) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ChetwoodForest = ChetwoodForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

chetwoodForest :: LocationCard ChetwoodForest
chetwoodForest = location ChetwoodForest Cards.chetwoodForest

-- | Travel: One player chooses and engages an enemy in the staging area.
-- Shadow: Defending player either exhausts a hero he controls, or returns attacking enemy
-- to the staging area after this attack.
instance RunMessage ChetwoodForest where
  runMessage msg l@(ChetwoodForest a) = case msg of
    TravelTo lid | lid == a.id -> do
      enemies <- select EnemyInStagingArea
      lead <- getLead
      unless (null enemies) $ choosePlayerM lead a \p -> chooseOrRunOneM p do
        questionLabeled "chetwoodForest.engage"
        causedBy a
        targets enemies (engageEnemy p)
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOrRunOneM pid do
        questionLabeled "chetwoodForest.choose"
        causedBy a
        unless (null heroes) $ labeled "chetwoodForest.exhaustHero" $ chooseAndExhaustHero a pid
        labeled "chetwoodForest.returnEnemy" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> ChetwoodForest <$> runMessage msg a
