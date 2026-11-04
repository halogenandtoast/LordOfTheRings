module LordOfTheRings.Location.Cards.SecretEntrance (secretEntrance) where

import LordOfTheRings.Card (CardDef (cdCardType), CardType (EnemyType, LocationType, TreacheryType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.ControlledLocation (returnToStaging)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (getTheLonelyMountain)

newtype SecretEntrance = SecretEntrance LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

secretEntrance :: LocationCard SecretEntrance
secretEntrance = location SecretEntrance Cards.secretEntrance

-- | While Secret Entrance is the active location, The Lonely Mountain's threat is reduced
-- by half (round up). Travel: the first player must have a hero to exhaust.
instance HasModifiersFor SecretEntrance where
  getModifiersFor (SecretEntrance a) = do
    when (a.placement == ActiveLocation) do
      mountain <- getTheLonelyMountain
      for_ mountain \l -> modified a l [ThreatHalved]
    lead <- getLead
    canExhaust <- selectAny (HeroControlledBy (PlayerWithId lead) <> ReadyHero)
    unless canExhaust $ modifySelf a [CannotBeTraveledTo]

-- | Once the players have traveled here, the top card of the encounter deck is revealed.
instance HasAbilities SecretEntrance where
  getAbilities (SecretEntrance a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

{- | Travel: The first player must exhaust a hero and name a card type to travel here.
Then, reveal the top card of the encounter deck. If the revealed card is not the named
type, return Secret Entrance to the staging area. (The players cannot travel again this
round: the travel phase has had its travel.) The types its encounter deck holds are
named, as Minas Tirith Lampwright's. The type named is kept on Secret Entrance
until the card is revealed, once it is the active location.
-}
instance RunMessage SecretEntrance where
  runMessage msg l@(SecretEntrance a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      chooseAndExhaustHero a lead
      chooseOneM lead do
        questionLabeled "secretEntrance.name"
        causedBy a
        for_ [(EnemyType, "enemy"), (LocationType, "location"), (TreacheryType, "treachery")] \(t, key) ->
          labeled ("secretEntrance." <> key) $ phaseModifier a a (NamedCardType t)
      SecretEntrance <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      named <- mapMaybe (\case NamedCardType t -> Just t; _ -> Nothing) <$> getModifiers a
      top <- getsGame (headMay . (.encounterDeck) . toAttrs . (.scenario))
      revealEncounterCard
      unless (maybe False ((`elem` named) . cdCardType . toCardDef) top) $ returnToStaging a.id
      pure l
    _ -> SecretEntrance <$> runMessage msg a
