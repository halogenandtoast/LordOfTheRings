module LordOfTheRings.Treachery.Cards.DolGuldurTreacheries
  (servantOfSauronDolGuldur, powerSevenfold, theNecromancersRage, fearAndDespairDolGuldur, fastnessOfMirkwood) where

import LordOfTheRings.Card (CardType (EnemyType, LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers (cityStrength)
import LordOfTheRings.Trait (Trait (DolGuldur, Orc))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DolGuldurTreachery = DolGuldurTreachery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

servantOfSauronDolGuldur, powerSevenfold, theNecromancersRage, fearAndDespairDolGuldur, fastnessOfMirkwood :: TreacheryCard DolGuldurTreachery
servantOfSauronDolGuldur = treachery DolGuldurTreachery Cards.servantOfSauronDolGuldur
powerSevenfold = treachery DolGuldurTreachery Cards.powerSevenfold
theNecromancersRage = treachery DolGuldurTreachery Cards.theNecromancersRage
fearAndDespairDolGuldur = treachery DolGuldurTreachery Cards.fearAndDespairDolGuldur
fastnessOfMirkwood = treachery DolGuldurTreachery Cards.fastnessOfMirkwood

instance RunMessage DolGuldurTreachery where
  runMessage msg t@(DolGuldurTreachery a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      if
        | toCardCode a == toCardCode Cards.servantOfSauronDolGuldur -> do
            enemies <- select EnemyInStagingArea
            costs <- traverse (\eid -> (eid,) <$> getEngagementCost eid) enemies
            let highest = [eid | (eid, value) <- costs, Just value == maximumMay (map snd costs)]
            lead <- getLead
            chooseOrRunOneM lead do
              questionLabeled "dolGuldur.servant"
              causedBy a
              targets highest $ immediateAttack lead
        | toCardCode a == toCardCode Cards.powerSevenfold -> eachPlayer $ \pid -> push $ WithTarget (PlayerTarget pid) $ DoStep 1 msg
        | toCardCode a == toCardCode Cards.theNecromancersRage -> do
            characters <- selectCharacters ExhaustedHero ExhaustedAlly
            for_ characters $ \target -> dealDamage a target 1
            phaseModifier a GameTarget $ EachDamagedCharacter $ WillpowerModifier (-1)
        | toCardCode a == toCardCode Cards.fearAndDespairDolGuldur -> do
            eachPlayer $ \pid -> chooseAndExhaust a pid 1
            roundModifier a GameTarget CannotBeReadiedByPlayerCards
        | otherwise -> do
            traverse_ (\target -> roundModifier a target $ ProgressPerRoundLimit 5) =<< getCurrentQuest
            whenM ((<= 5) <$> cityStrength) $ phaseModifier a a $ GainKeyword Surge
      pure t
    WithTarget (PlayerTarget pid) (DoStep 1 (WhenRevealed (isTarget a -> True))) -> do
      available <- getsGame $ \g -> let attrs = toAttrs g.scenario in attrs.encounterDeck <> attrs.encounterDiscard
      let locationMatcher = CardWithType LocationType <> CardWithTrait DolGuldur
          enemyMatcher = CardWithType EnemyType <> CardWithTrait Orc
      chooseOrRunOneM pid do
        questionLabeled "dolGuldur.powerSevenfold"
        causedBy a
        when (any (cardMatches locationMatcher) available) $
          labeled "dolGuldur.searchLocation" $ searchEncounterDeckAndDiscard pid a locationMatcher
        when (any (cardMatches enemyMatcher) available) $
          labeled "dolGuldur.searchOrc" $ searchEncounterDeckAndDiscardToEngage pid a enemyMatcher
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      if
        | toCardCode a == toCardCode Cards.servantOfSauronDolGuldur -> additionalAttack pid eid
        | toCardCode a == toCardCode Cards.theNecromancersRage -> for_ defenders $ \target -> do
            dealDamage (CardIdSource cid) target 1
            phaseModifier (CardIdSource cid) target $ DefensePerDamage (-1)
        | toCardCode a == toCardCode Cards.fearAndDespairDolGuldur -> for_ defenders $ \target -> roundModifier (CardIdSource cid) target CannotReady
        | otherwise -> do
            progress <- getMainQuestProgress
            let bonus = if progress >= 5 then 2 else 1
            phaseModifiers (CardIdSource cid) eid [AttackModifier bonus, DefenseModifier bonus]
      pure t
    _ -> DolGuldurTreachery <$> runMessage msg a
