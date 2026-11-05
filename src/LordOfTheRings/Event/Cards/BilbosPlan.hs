module LordOfTheRings.Event.Cards.BilbosPlan (bilbosPlan) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationThreat)

newtype BilbosPlan = BilbosPlan EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Played after an attack by your characters destroys an enemy.
bilbosPlan :: EventCard BilbosPlan
bilbosPlan =
  eventWith BilbosPlan Cards.bilbosPlan \a ->
    let you = PlayerWithId a.owner
     in a & windowL ?~ EnemyDestroyedByCharacterWindow After (HeroControlledBy you) (AllyControlledBy you)

-- | Response: After you attack and destroy an enemy, shuffle the encounter discard pile into
-- the encounter deck and discard cards from the top until an enemy is discarded. Add that
-- enemy to the staging area to discard a non-unique location with threat equal to or less
-- than that enemy's threat.
--
-- Adding the enemy is the cost of discarding the location ("do X to do Y"), so it is only
-- offered with a location to discard, and the player may decline it; the enemy then stays
-- in the discard pile. It is compared by its printed threat, the threat it would have once
-- added. The shuffled deck is read back in a later step.
instance RunMessage BilbosPlan where
  runMessage msg e@(BilbosPlan attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure e
    DoStep 1 (PlayEvent pid card _) | card.id == toId attrs -> do
      discardEncounterCardsUntilThen attrs (CardWithType EnemyType) \enemy -> do
        let threat = fromMaybe 0 (cdThreat $ toCardDef enemy)
        candidates <- select (not_ UniqueLocation <> LocationDiscardableByPlayerCards)
        locations <- filterM (fmap (<= threat) . getLocationThreat) candidates
        chooseOneM pid do
          questionLabeled "bilbosPlan.location"
          causedBy attrs
          targets locations \lid -> do
            addToStagingFromDiscard enemy
            discard attrs lid
          unless (null locations) $ labeled "bilbosPlan.skip" (pure ())
      pure e
    _ -> BilbosPlan <$> runMessage msg attrs
