module LordOfTheRings.Treachery.Cards.LostAndAlone (lostAndAlone) where

import LordOfTheRings.Trait (Trait (Nameless))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LostAndAlone = LostAndAlone TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostAndAlone :: TreacheryCard LostAndAlone
lostAndAlone = treachery LostAndAlone Cards.lostAndAlone

-- | When Revealed: Each player chooses and shuffles a hero he controls into his deck. When
-- he draws that hero, he puts it into play. (A hero is put into play as it is drawn.)
-- Shadow: If attacking enemy is Nameless, the defending player must discard his hand.
instance RunMessage LostAndAlone where
  runMessage msg t@(LostAndAlone attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "lostAndAlone.hero"
          causedBy attrs
          targets heroes shuffleIntoOwnersDeck
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Nameless) $ discardHand pid
      pure t
    _ -> LostAndAlone <$> runMessage msg attrs
