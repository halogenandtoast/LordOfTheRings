module LordOfTheRings.Enemy.Cards.WitchOfAngmar (witchOfAngmar) where

import LordOfTheRings.Card (CardDef (cdShadowEffect), EncounterCard, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Sorcery))

newtype WitchOfAngmar = WitchOfAngmar EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

witchOfAngmar :: EnemyCard WitchOfAngmar
witchOfAngmar = enemy WitchOfAngmar Cards.witchOfAngmar

{- | Forced: When you resolve shadow cards dealt to Witch of Angmar, if they have the
Sorcery trait, resolve their "when revealed" effects. (Resolve any shadow effects first.)
A Sorcery card with a shadow effect does so once that effect has resolved; one without,
as it turns face up. It leaves the shadow cards to resolve, as if revealed.
-}
instance RunMessage WitchOfAngmar where
  runMessage msg (WitchOfAngmar attrs) = case msg of
    RevealShadowCard eid cid | eid == attrs.id, Just card <- sorcery cid, not (cdShadowEffect $ toCardDef card) -> do
      revealed card
      WitchOfAngmar <$> runMessage msg attrs
    CheckWindows ws | cards@(_ : _) <- [card | Window After (ShadowEffectResolved cid eid) <- ws, eid == attrs.id, Just card <- [sorcery cid]] -> do
      traverse_ revealed cards
      WitchOfAngmar <$> runMessage msg attrs
    _ -> WitchOfAngmar <$> runMessage msg attrs
   where
    sorcery :: CardId -> Maybe EncounterCard
    sorcery cid = find (\c -> c.id == cid && Sorcery `member` (toCardDef c).traits) attrs.shadowCards
    revealed card = do
      push $ RemoveShadowCard attrs.id card.id
      resolveWhenRevealedOf card
