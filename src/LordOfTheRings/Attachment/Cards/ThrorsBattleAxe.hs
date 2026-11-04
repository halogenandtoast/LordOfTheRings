module LordOfTheRings.Attachment.Cards.ThrorsBattleAxe (throrsBattleAxe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrorsBattleAxe = ThrorsBattleAxe AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Thrór's Battle Axe of The Lonely Mountain. Attach to a hero. Restricted, on
-- its card def.
throrsBattleAxe :: AttachmentCard ThrorsBattleAxe
throrsBattleAxe = attachment ThrorsBattleAxe Cards.throrsBattleAxe

-- | Attached hero gets +2 [attack].
instance HasModifiersFor ThrorsBattleAxe where
  getModifiersFor (ThrorsBattleAxe a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached hero attacks and destroys an enemy, deal 1 damage to another
-- enemy engaged with you. (The one destroyed is gone from play.)
instance HasAbilities ThrorsBattleAxe where
  getAbilities (ThrorsBattleAxe a) =
    [ restricted a 1 (exists $ EnemyEngagedWith (PlayerWithId a.controller) <> EnemyNotImmuneToPlayerCards)
        $ response
        $ EnemyDestroyedByAttackWindow After (Just host)
    | AttachedTo host@(HeroTarget _) <- [a.placement]
    ]

instance RunMessage ThrorsBattleAxe where
  runMessage msg s@(ThrorsBattleAxe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith (PlayerWithId pid) <> EnemyNotImmuneToPlayerCards)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> ThrorsBattleAxe <$> runMessage msg a
