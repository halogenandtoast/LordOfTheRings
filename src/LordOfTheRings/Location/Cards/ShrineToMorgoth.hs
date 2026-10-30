module LordOfTheRings.Location.Cards.ShrineToMorgoth (shrineToMorgoth) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Undead))

-- | The Fate of Númenor's Shrine to Morgoth: Uncharted, under a Lost Island side.
newtype ShrineToMorgoth = ShrineToMorgoth LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

shrineToMorgoth :: LocationCard ShrineToMorgoth
shrineToMorgoth = location ShrineToMorgoth Cards.shrineToMorgoth

-- | While Shrine to Morgoth is the active location, players cannot draw cards or search
-- their decks.
instance HasModifiersFor ShrineToMorgoth where
  getModifiersFor (ShrineToMorgoth a) = when (a.placement == ActiveLocation) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotDrawCards, CannotSearchDeck]

-- | Forced: After Shrine to Morgoth is flipped over while active, exhaust each ally in play
-- with printed cost 2 or less. Then, each player must search the encounter deck and
-- discard pile for an Undead enemy and put it into play engaged with him. Shuffle the
-- encounter deck.
instance HasAbilities ShrineToMorgoth where
  getAbilities (ShrineToMorgoth a) =
    [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage ShrineToMorgoth where
  runMessage msg l@(ShrineToMorgoth a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      -- an objective ally too, its cost printed as none: Curse of the Downfallen names "non-objective"
      allies <- select ReadyAlly
      cheap <- filterM (fmap ((<= 2) . printedCostOf) . getAlly) allies
      traverse_ exhaust cheap
      eachPlayer \p -> searchEncounterDeckAndDiscardToEngage p a (CardWithType EnemyType <> CardWithTrait Undead)
      pure l
    _ -> ShrineToMorgoth <$> runMessage msg a
