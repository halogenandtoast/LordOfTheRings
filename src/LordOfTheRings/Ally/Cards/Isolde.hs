module LordOfTheRings.Ally.Cards.Isolde (isolde) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Trait (Trait (Item))

newtype Isolde = Isolde AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Ered Mithrin campaign, from The King's Quest.
isolde :: AllyCard Isolde
isolde = ally Isolde Cards.isolde

-- | Isolde gets +2 [willpower] while she has an Item attachment.
instance HasModifiersFor Isolde where
  getModifiersFor (Isolde a) = do
    items <- getsGame \g -> [x | x <- toList g.entities.attachments, (toAttrs x).placement == AttachedTo (toTarget a), Item `member` cdCardTraits (toCardDef x)]
    when (notNull items) $ modifySelf a [WillpowerModifier 2]

-- | Response: After you engage an enemy, ready Isolde.
instance HasAbilities Isolde where
  getAbilities (Isolde a) =
    [restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly) $ response $ PlayerEngagedEnemyWindow After pid | Just pid <- [placementController a.placement]]

instance RunMessage Isolde where
  runMessage msg s@(Isolde a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> Isolde <$> runMessage msg a
