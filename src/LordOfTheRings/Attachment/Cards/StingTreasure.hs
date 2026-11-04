module LordOfTheRings.Attachment.Cards.StingTreasure (stingTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)

newtype StingTreasure = StingTreasure AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Sting of We Must Away, Ere Break of Day. Attach to Bilbo Baggins.
-- Restricted, on its card def.
stingTreasure :: AttachmentCard StingTreasure
stingTreasure = attachment StingTreasure Cards.stingTreasure & attachTo (AttachToHero $ HeroWithTitle "Bilbo Baggins")

-- | Bilbo Baggins gets +1 [willpower], +1 [attack], and +1 [defense].
instance HasModifiersFor StingTreasure where
  getModifiersFor (StingTreasure a) = withHost a \host -> modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

-- | Response: After Bilbo Baggins exhausts to defend, discard the top card of the encounter
-- deck. Deal damage to the attacking enemy equal to the discarded card's [threat].
instance HasAbilities StingTreasure where
  getAbilities (StingTreasure a) =
    [ restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero)
        $ response
        $ CharacterDefenderDeclaredWindow After (HeroWithId hid) (not_ AnyAlly) AnyEnemy
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage StingTreasure where
  runMessage msg s@(StingTreasure a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      top <- getTopOfEncounterDeck
      discardTopOfEncounterDeck a 1
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid ->
        for_ (cdThreat . toCardDef =<< top) \x -> when (x > 0) $ dealDamage a eid x
      pure s
    _ -> StingTreasure <$> runMessage msg a
