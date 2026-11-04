module LordOfTheRings.Event.Cards.ProudHunters (proudHunters) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (AttackDestroyed (..), gameAttackDestroyed)
import LordOfTheRings.Trait (Trait (Noble, Ranger))

newtype ProudHunters = ProudHunters EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Noble trait and another unique
-- character with the Ranger trait. Response: After a hero you control participates in an
-- attack that destroys an enemy, add X resources to that hero's pool. X is the just
-- destroyed enemy's printed threat.
proudHunters :: EventCard ProudHunters
proudHunters =
  eventWith ProudHunters Cards.proudHunters \a ->
    let you = PlayerWithId a.owner
        unique trait = (HeroControlledBy you <> HeroWithTrait trait, AllyControlledBy you <> UniqueAlly <> AllyWithTrait trait)
     in a
          & windowL ?~ EnemyDestroyedByHeroWindow After (HeroControlledBy you)
          & criteriaL .~ DistinctCharacters [unique Noble, unique Ranger]

instance RunMessage ProudHunters where
  runMessage msg e@(ProudHunters attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      mine <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
      -- the enemy has left play: its card is kept with the attack that destroyed it
      destroyed <- getsGame gameAttackDestroyed
      let printedThreat eid = sum [fromMaybe 0 (cdThreat $ toCardDef d.attackDestroyedCard) | d <- take 1 (filter ((== EnemyTarget eid) . (.target)) destroyed)]
          heroes = [(h, printedThreat eid) | Window _ (EnemyDestroyedByAttack eid attackers _) <- windows, h <- attackers, h `elem` mine]
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroForResource"
        causedBy attrs
        for_ heroes \(h, x) -> targeting h $ placeResources attrs h x
      pure e
    _ -> ProudHunters <$> runMessage msg attrs
