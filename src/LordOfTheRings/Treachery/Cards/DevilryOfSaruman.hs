module LordOfTheRings.Treachery.Cards.DevilryOfSaruman (devilryOfSaruman) where

import LordOfTheRings.Keyword (Keyword (Toughness))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DevilryOfSaruman = DevilryOfSaruman TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

devilryOfSaruman :: TreacheryCard DevilryOfSaruman
devilryOfSaruman = treachery DevilryOfSaruman Cards.devilryOfSaruman

{- | When Revealed: Place 3 progress on the current quest (bypassing any active location).
Until the end of the round, treat each character's text box as if it were blank (except
for Traits): each character in play as it resolves.

Shadow: Attacking enemy gets +2 attack and gains Toughness 2 until the end of the round.
-}
instance RunMessage DevilryOfSaruman where
  runMessage msg t@(DevilryOfSaruman attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeProgressOnCurrentQuest attrs 3
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifier attrs c Blank
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifiers attrs eid [AttackModifier 2, GainKeyword (Toughness 2)]
      pure t
    _ -> DevilryOfSaruman <$> runMessage msg attrs
