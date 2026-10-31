module LordOfTheRings.Treachery.Cards.Backtrack (backtrack) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType, LocationType), toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Backtrack = Backtrack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

backtrack :: TreacheryCard Backtrack
backtrack = treachery Backtrack Cards.backtrack

-- | Surge.
-- When Revealed: The topmost enemy or location card in the encounter discard pile is
-- returned to the staging area.
-- Shadow: Deal and resolve the topmost Shadow effect in the encounter discard pile for this
-- attack.
instance RunMessage Backtrack where
  runMessage msg t@(Backtrack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardPile <- getEncounterDiscard
      for_ (find ((`elem` [EnemyType, LocationType]) . cdCardType . toCardDef) discardPile) addToStagingFromDiscard
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      discardPile <- getEncounterDiscard
      for_ (find (\c -> cdShadowEffect (toCardDef c) && c.id /= cid) discardPile)
        $ dealAndResolveShadowCard eid pid defenders
      pure t
    _ -> Backtrack <$> runMessage msg attrs
