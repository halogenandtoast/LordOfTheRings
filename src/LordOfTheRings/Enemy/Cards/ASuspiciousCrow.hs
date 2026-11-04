module LordOfTheRings.Enemy.Cards.ASuspiciousCrow (aSuspiciousCrow) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ASuspiciousCrow = ASuspiciousCrow EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aSuspiciousCrow :: EnemyCard ASuspiciousCrow
aSuspiciousCrow = enemy ASuspiciousCrow Cards.aSuspiciousCrow

{- | When Revealed: Reveal the top card of the encounter discard pile and add it to the
staging area, if able.
Shadow: If this attack deals at least 1 damage, shuffle this card back into the encounter
deck: read off the damage the attack goes on to deal.
-}
instance RunMessage ASuspiciousCrow where
  runMessage msg e@(ASuspiciousCrow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ revealFromEncounterDiscard . headMay =<< getEncounterDiscard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      characters <- selectCharacters AnyHero AnyAlly
      shuffled <- capture $ shuffleShadowCardIntoEncounterDeck eid (MkEncounterCard cid (toCardCode attrs))
      afterAttack eid [IfAttackDamaged eid characters shuffled]
      pure e
    _ -> ASuspiciousCrow <$> runMessage msg attrs
