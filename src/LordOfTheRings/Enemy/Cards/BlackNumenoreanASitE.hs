module LordOfTheRings.Enemy.Cards.BlackNumenoreanASitE (blackNumenoreanASitE) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype BlackNumenoreanASitE = BlackNumenoreanASitE EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

blackNumenoreanASitE :: EnemyCard BlackNumenoreanASitE
blackNumenoreanASitE = enemy BlackNumenoreanASitE Cards.blackNumenoreanASitE

-- | Immune to player card effects.
instance HasModifiersFor BlackNumenoreanASitE where
  getModifiersFor (BlackNumenoreanASitE a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: When Black Númenórean attacks, resolve each of his faceup shadow cards as if
-- they had been dealt this round.
instance HasAbilities BlackNumenoreanASitE where
  getAbilities (BlackNumenoreanASitE a) =
    [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id | notNull (faceup a)]

-- | Those turned faceup by an earlier attack, not those dealt faceup and still to resolve.
faceup :: EnemyAttrs -> [CardId]
faceup a = [c.id | c <- a.shadowCards, c.id `elem` a.enemyShadowRevealed, c.id `notElem` a.enemyShadowFaceUp]

{- | Do not discard shadow cards dealt to Black Númenórean if they have a shadow effect:
they stay faceup on him as the combat phase ends, or as an attack outside it discards them.
The Forced effect turns each faceup one up again to resolve with the attack's shadow cards.
-}
instance RunMessage BlackNumenoreanASitE where
  runMessage msg e@(BlackNumenoreanASitE attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (faceup attrs) (push . TurnShadowCardFaceUp attrs.id)
      pure e
    EndPhase CombatPhase -> keepShadowEffects
    DiscardShadowCards eid | eid == attrs.id -> keepShadowEffects
    _ -> BlackNumenoreanASitE <$> runMessage msg attrs
   where
    keepShadowEffects = do
      let (kept, gone) = partition (cdShadowEffect . toCardDef) attrs.shadowCards
          ids = map (.id) kept
      pushWhen (notNull gone) $ AddToEncounterDiscard gone
      -- each is faceup now: one dealt this round turned over as it resolved
      pure
        $ BlackNumenoreanASitE
        $ attrs
        & shadowCardsL
        .~ kept
        & shadowRevealedL
        .~ ids
        & shadowFaceUpL
        .~ []
