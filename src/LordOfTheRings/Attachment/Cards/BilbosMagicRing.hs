module LordOfTheRings.Attachment.Cards.BilbosMagicRing (bilbosMagicRing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BilbosMagicRing = BilbosMagicRing AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure of Flies and Spiders. Attach to Bilbo Baggins.
bilbosMagicRing :: AttachmentCard BilbosMagicRing
bilbosMagicRing = attachment BilbosMagicRing Cards.bilbosMagicRing & attachTo (AttachToHero $ HeroWithTitle "Bilbo Baggins")

{- | Action: Exhaust Bilbo's Magic Ring and raise your threat by 1 to gain 1 [baggins]
resource. (Limit once per round.)
Response: After Bilbo Baggins exhausts to defend an attack, exhaust Bilbo's Magic Ring and
raise your threat by 3 to cancel all damage from this attack.

The [baggins] resource goes to the pool of Bilbo Baggins, the Baggins hero it is attached
to; a treasure's, it is one On the Doorstep's Bilbo can gain.
-}
instance HasAbilities BilbosMagicRing where
  getAbilities (BilbosMagicRing a) = case a.placement of
    AttachedTo (HeroTarget hid) ->
      [ limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (Costs [exhaustCost a, RaiseThreatCost 1])
      , restricted a 2 (exists $ HeroWithId hid <> ExhaustedHero)
          $ ResponseAbility
            (CharacterDefenderDeclaredWindow After (HeroWithId hid) (not_ AnyAlly) AnyEnemy)
            (Costs [exhaustCost a, RaiseThreatCost 3])
      ]
    _ -> []

instance RunMessage BilbosMagicRing where
  runMessage msg s@(BilbosMagicRing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid ->
        createEffect a GameTarget [AttackDamageCanceled eid] UntilEndOfAttack
      pure s
    _ -> BilbosMagicRing <$> runMessage msg a
