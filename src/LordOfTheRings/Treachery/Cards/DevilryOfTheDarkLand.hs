module LordOfTheRings.Treachery.Cards.DevilryOfTheDarkLand (devilryOfTheDarkLand) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Trait (Trait (Undead))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DevilryOfTheDarkLand = DevilryOfTheDarkLand TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

devilryOfTheDarkLand :: TreacheryCard DevilryOfTheDarkLand
devilryOfTheDarkLand = treachery DevilryOfTheDarkLand Cards.devilryOfTheDarkLand

-- | When Revealed: Return the top X Undead enemies in the encounter discard pile to the
-- staging area, where X is the number of players. If no enemies were returned to the
-- staging area as a result of this effect, Devilry of the Dark Land gains surge.
instance RunMessage DevilryOfTheDarkLand where
  runMessage msg t@(DevilryOfTheDarkLand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      let undead c = cdCardType (toCardDef c) == EnemyType && Undead `member` cdCardTraits (toCardDef c)
      found <- take players . filter undead <$> getEncounterDiscard
      if null found then revealEncounterCard else traverse_ addToStagingFromDiscard found
      pure t
    _ -> DevilryOfTheDarkLand <$> runMessage msg attrs
