module LordOfTheRings.Attachment.Cards.StingTheRoadDarkens (stingTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Trait (Trait (Hobbit))

newtype StingTheRoadDarkens = StingTheRoadDarkens AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Road Darkens. Attach to a Hobbit hero. Restricted, on its card def.
stingTheRoadDarkens :: AttachmentCard StingTheRoadDarkens
stingTheRoadDarkens = attachment StingTheRoadDarkens Cards.stingTheRoadDarkens & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | Attached hero gets +1 [willpower], +1 [attack], and +1 [defense].
instance HasModifiersFor StingTheRoadDarkens where
  getModifiersFor (StingTheRoadDarkens a) = withHost a \host -> modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

-- | Response: After attached hero exhausts to defend an attack, discard the top card of the
-- encounter deck. Deal damage to the attacking enemy equal to the discarded card's
-- [threat]. Exhausted as it is declared, it exhausted to defend.
instance HasAbilities StingTheRoadDarkens where
  getAbilities (StingTheRoadDarkens a) =
    [ restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero) $ response $ DefenderDeclaredWindow After (Just host)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage StingTheRoadDarkens where
  runMessage msg s@(StingTheRoadDarkens a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid ->
        discardTopOfEncounterDeckFor a eid 1
      pure s
    DiscardedTopOfEncounterDeck (isSource a -> True) (EnemyTarget eid) cards -> do
      let threat = sum [fromMaybe 0 (cdThreat $ toCardDef c) | c <- cards]
      when (threat > 0) $ dealDamage a eid threat
      pure s
    _ -> StingTheRoadDarkens <$> runMessage msg a
