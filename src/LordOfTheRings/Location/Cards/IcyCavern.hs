module LordOfTheRings.Location.Cards.IcyCavern (icyCavern) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Snow))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype IcyCavern = IcyCavern LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

icyCavern :: LocationCard IcyCavern
icyCavern = location IcyCavern Cards.icyCavern

-- | X is the number of Snow cards in the staging area.
-- With no Freezing Cold in the encounter deck or discard pile, its Travel cost cannot be
-- paid.
instance HasModifiersFor IcyCavern where
  getModifiersFor (IcyCavern a) = do
    locations <- length <$> select (LocationInStagingArea <> LocationWithTrait Snow)
    enemies <- length <$> select (EnemyInStagingArea <> EnemyWithTrait Snow)
    treacheries <-
      getsGame (length . filter (\t -> t.placement == StagingArea && cardMatches (CardWithTrait Snow) t) . toList . (.entities.treacheries))
    found <- getsGame \g -> let s = toAttrs g.scenario in any (cardMatches $ cardIs Treacheries.freezingCold) (s.encounterDeck <> s.encounterDiscard)
    modified a a $ [ThreatModifier x | let x = locations + enemies + treacheries, x > 0] <> [CannotBeTraveledTo | not found]

-- | Travel: Search the encounter deck and discard pile for 1 copy of Freezing Cold and
-- reveal it to travel here. Shuffle the encounter deck.
-- Shadow: Defending character gets -1 [Willpower]. Deal the attacking enemy 1 additional
-- shadow card.
instance RunMessage IcyCavern where
  runMessage msg l@(IcyCavern a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToReveal lead a (cardIs Treacheries.freezingCold)
      IcyCavern <$> runMessage msg a
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      for_ defenders \d -> attackModifier a d (WillpowerModifier (-1))
      dealShadowCardsDuringAttack eid pid defenders 1
      pure l
    _ -> IcyCavern <$> runMessage msg a
